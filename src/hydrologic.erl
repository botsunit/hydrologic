-module(hydrologic).
-include("../include/hydrologic.hrl").

-export([
         new/2,
         run/2,
         flow/2,
         stop/1
        ]).

-type operation() :: term().
-type pipe() :: atom().
-type data() :: any().

% @doc
% Create a new pipe.
% @end
-spec new(atom(), [operation()]) -> pipe().
new(Name, Operations) when is_list(Operations) ->
  create(Name, Operations, self()).

% @doc
%
% @end
-spec run(pipe(), data()) -> {ok, data()}
                             | {error, term(), data()}
                             | {error, term()}
                             | any().
run(Pipe, Data) ->
  case ets:lookup(Pipe, config) of
    [{config, #{start := StartPID}}] when ?IS_RECEIVER(StartPID) ->
      StartPID ! #{
        name => Pipe,
        uuid => uuid:to_string(uuid:uuid4()),
        fan => -1,
        flow => bucs:is_list_of_lists(Data),
        data => Data,
        error => none},
      receive
        #{error := none, data := Response} ->
          {ok, Response};
        #{error := Error, data := Response} ->
          {error, Error, Response};
        Other ->
          Other
      end;
    _ ->
      {error, invalid_pipe}
  end.

% @equiv run(Pipe, Data)
flow(Pipe, Data) ->
  run(Pipe, Data).

% @doc
% Destroy the given pipe.
% @end
-spec stop(pipe()) -> ok | {error, term()}.
stop(Pipe) ->
  case ets:lookup(Pipe, config) of
    [{config, #{processes := ProcessPIDs,
                name := Name}}] ->
      [begin
         case is_pid(PID) of
           true ->
             erlang:exit(PID, normal);
           false ->
             erlang:exit(erlang:whereis(PID), normal),
             erlang:unregister(PID)
         end
       end || PID <- ProcessPIDs],
      ets:delete(Name),
      ok;
    _ ->
      {error, invalid_pipe}
  end.

create(Name, Operations, PID) ->
  {StartPID, ProcessPIDs} = lists:foldr(fun(Worker, {Receiver, PIDs}) ->
                                            WPID = make_worker(Worker, Receiver, PID),
                                            {WPID, [WPID|PIDs]}
                                        end, {PID, []}, Operations),
  ets:new(Name, [named_table, public]),
  ets:insert(Name, {config, #{name => Name,
                              final => PID,
                              start => StartPID,
                              processes => ProcessPIDs}}),
  Name.

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

worker({merge, Function}, Pid, EndPID) when ?IS_RECEIVER(Pid),
                                            ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    #{fan := -1} = Record ->
      Pid ! Record,
      worker(merge, Pid, EndPID);
    Record ->
      case merge(Record, Pid, Function) of
        ok ->
          worker({merge, Function}, Pid, EndPID);
        error ->
          Pid ! eof % TODO: stop the stream
      end
  end;
worker({duplicate, AltPid}, Pid, EndPID) when ?IS_RECEIVER(AltPid),
                                           ?IS_RECEIVER(Pid),
                                           ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    Record ->
      AltPid ! Record#{fan => 1},
      Pid ! Record#{fan => 0},
      worker({duplicate, AltPid}, Pid, EndPID)
  end;
worker(Fun, Pid, EndPID) when is_function(Fun, 1),
                              ?IS_RECEIVER(Pid),
                              ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      Pid ! eof;
    #{data := Data} = Record ->
      response(
        Pid,
        EndPID,
        Record,
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
    #{data := Data} = Record ->
      response(
        Pid,
        AltPid,
        EndPID,
        Record,
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
    #{data := Data} = Record ->
      response(
        Pid,
        EndPID,
        Record,
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
    #{data := Data} = Record ->
      response(
        Pid,
        AltPid,
        EndPID,
        Record,
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

response(Pid, EndPID, Record, Response) ->
  case Response of
    {map, NewData} ->
      Pid ! Record#{data => NewData};
    {reduce, true} ->
      Pid ! Record;
    {reduce, false} ->
      EndPID ! ok;
    {return, NewData} ->
      EndPID ! Record#{data => NewData};
    {error, Error} ->
      EndPID ! Record#{error => Error};
    Other ->
      Pid ! Record#{data => Other}
  end.
response(Pid, AltPid, EndPID, Record, Response) ->
  case Response of
    {map, NewData} ->
      Pid ! Record#{data => NewData};
    {reduce, true} ->
      Pid ! Record;
    {reduce, false} ->
      AltPid ! Record;
    {return, NewData} ->
      EndPID ! Record#{data => NewData};
    {error, Error} ->
      EndPID ! Record#{error => Error};
    Other ->
      Pid ! Record#{data => Other}
  end.

gosub(Receiver, Pid) ->
  receive
    eof ->
      Pid ! eof;
    Data ->
      Receiver ! Data,
      gosub(Receiver, Pid)
  end.

merge(#{fan := Fan1, uuid := UUID, data := Data1, name := Name, flow := Flow} = Record, Pid, Function) ->
  case ets:lookup(Name, UUID) of
    [{UUID, #{fan := Fan0, uuid := UUID, data := Data0, name := Name, flow := Flow}}] ->
      ets:delete(Name, UUID),
      NewData = mergefun(Function,
                         case Fan0 < Fan1 of
                           true ->
                             [Data0, Data1];
                           false ->
                             [Data1, Data0]
                         end),
      Pid ! #{name => Name,
              uuid => UUID,
              fan => -1,
              flow => Flow,
              data => NewData,
              error => none},
      ok;
    [] ->
      ets:insert(Name, {UUID, Record}),
      ok;
    _ ->
      error
  end.

mergefun({Module, Function}, Args) ->
  erlang:apply(Module, Function, Args);
mergefun(Function, Args) ->
  erlang:apply(Function, Args).
