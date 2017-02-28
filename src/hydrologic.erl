-module(hydrologic).
-include("../include/hydrologic.hrl").

-export([
         new/1,
         run/2,
         stop/1
        ]).

new(Operations) when is_list(Operations) ->
  create(Operations, self()).

run(#{start := StartPID}, Data) when ?IS_RECEIVER(StartPID) ->
  StartPID ! Data,
  receive
    X -> X
  end.

stop(#{processes := ProcessPIDs}) ->
  [begin
     case is_pid(PID) of
       true ->
         erlang:exit(PID, normal);
       false ->
         erlang:exit(erlang:whereis(PID), normal),
         erlang:unregister(PID)
     end
   end || PID <- ProcessPIDs],
  ok.

create(Operations, PID) ->
  {StartPID, ProcessPIDs} = lists:foldr(fun(Worker, {Receiver, PIDs}) ->
                                            WPID = make_worker(Worker, Receiver, PID),
                                            {WPID, [WPID|PIDs]}
                                        end, {PID, []}, Operations),
  #{final => PID,
    start => StartPID,
    processes => ProcessPIDs}.

make_worker({Label, Worker}, Receiver, EndPID) when ?IS_RECEIVER(Label),
                                                    ?IS_WORKER(Worker),
                                                    ?IS_RECEIVER(Receiver),
                                                    is_pid(EndPID) ->
  WorkerPID = make_worker(Worker, Receiver, EndPID),
  erlang:register(Label, WorkerPID),
  Label;
make_worker(Worker, Receiver, EndPID) when ?IS_WORKER(Worker),
                                           ?IS_RECEIVER(Receiver),
                                           is_pid(EndPID) ->
  erlang:spawn_link(fun() ->
                        worker(Worker, Receiver, EndPID)
                    end).

worker(Fun, Pid, EndPID) when is_function(Fun, 1),
                              ?IS_RECEIVER(Pid),
                              ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    Data ->
      response(
        Pid,
        EndPID,
        Data,
        erlang:apply(Fun, [Data])),
      worker(Fun, Pid, EndPID)
  end;
worker({Fun, AltPid} = Worker, Pid, EndPID) when is_function(Fun, 1),
                                                 ?IS_RECEIVER(AltPid),
                                                 ?IS_RECEIVER(Pid),
                                                 ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    Data ->
      response(
        Pid,
        AltPid,
        EndPID,
        Data,
        erlang:apply(Fun, [Data])),
      worker(Worker, Pid, EndPID)
  end;
worker({Module, Function, Args} = Worker, Pid, EndPID) when is_atom(Module),
                                                            is_atom(Function),
                                                            is_list(Args),
                                                            ?IS_RECEIVER(Pid),
                                                            ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    Data ->
      response(
        Pid,
        EndPID,
        Data,
        erlang:apply(Module, Function, [Data|Args])),
      worker(Worker, Pid, EndPID)
  end;
worker({{Module, Function, Args}, AltPid} = Worker, Pid, EndPID) when is_atom(Module),
                                                                      is_atom(Function),
                                                                      is_list(Args),
                                                                      ?IS_RECEIVER(AltPid),
                                                                      ?IS_RECEIVER(Pid),
                                                                      ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    Data ->
      response(
        Pid,
        AltPid,
        EndPID,
        Data,
        erlang:apply(Module, Function, [Data|Args])),
      worker(Worker, Pid, EndPID)
  end;
worker({Module, Function}, Pid, EndPID) when is_atom(Module),
                                             is_atom(Function),
                                             ?IS_RECEIVER(Pid),
                                             ?IS_RECEIVER(EndPID) ->
  worker({Module, Function, []}, Pid, EndPID);
worker({{Module, Function}, AltPid}, Pid, EndPID) when is_atom(Module),
                                                       is_atom(Function),
                                                       ?IS_RECEIVER(AltPid),
                                                       ?IS_RECEIVER(Pid),
                                                       ?IS_RECEIVER(EndPID) ->
  worker({{Module, Function, []}, AltPid}, Pid, EndPID);
worker(FunctionOrReceiver, Pid, EndPID) when is_atom(FunctionOrReceiver),
                                             ?IS_RECEIVER(Pid),
                                             ?IS_RECEIVER(EndPID) ->
  case bucs:function_exists(hydrologic_stdlib, FunctionOrReceiver, 1) of
    true ->
      worker({hydrologic_stdlib, FunctionOrReceiver, []}, Pid, EndPID);
    false ->
      case whereis(FunctionOrReceiver) of
        undefined ->
          EndPID ! {error, {undefined, FunctionOrReceiver}};
        _ ->
          gosub(FunctionOrReceiver, Pid),
          worker(FunctionOrReceiver, Pid, EndPID)
      end
  end;
worker({Function, Args}, Pid, EndPID) when is_atom(Function),
                                           is_list(Args) ,
                                           ?IS_RECEIVER(Pid),
                                           ?IS_RECEIVER(EndPID) ->
  worker({hydrologic_stdlib, Function, Args}, Pid, EndPID);
worker({Function, AltPid}, Pid, EndPID) when is_atom(Function),
                                             ?IS_RECEIVER(AltPid),
                                             ?IS_RECEIVER(Pid),
                                             ?IS_RECEIVER(EndPID) ->
  worker({{hydrologic_stdlib, Function, []}, AltPid}, Pid, EndPID);
worker({{Function, Args}, AltPid}, Pid, EndPID) when is_atom(Function),
                                                     is_list(Args),
                                                     ?IS_RECEIVER(AltPid),
                                                     ?IS_RECEIVER(Pid),
                                                     ?IS_RECEIVER(EndPID) ->
  worker({{hydrologic_stdlib, Function, Args}, AltPid}, Pid, EndPID).

gosub(Receiver, Pid) ->
  receive
    eof ->
      Pid ! eof;
    Data ->
      Receiver ! Data,
      gosub(Receiver, Pid)
  end.

response(Pid, EndPID, Data, Response) ->
  case Response of
    {map, NewData} ->
      Pid ! NewData;
    {reduce, true} ->
      Pid ! Data;
    {reduce, false} ->
      EndPID ! ok;
    {return, NewData} ->
      EndPID ! NewData;
    {error, _} = Error ->
      EndPID ! Error;
    Other ->
      Pid ! Other
  end.
response(Pid, AltPid, EndPID, Data, Response) ->
  case Response of
    {map, NewData} ->
      Pid ! NewData;
    {reduce, true} ->
      Pid ! Data;
    {reduce, false} ->
      AltPid ! Data;
    {return, NewData} ->
      EndPID ! NewData;
    {error, _} = Error ->
      EndPID ! Error;
    Other ->
      Pid ! Other
  end.

