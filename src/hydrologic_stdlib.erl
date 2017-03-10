-module(hydrologic_stdlib).

-export([
         return/1
         , console/1
         , console/2
        ]).

-export([
         match/2
         , pad/3
         , pad/4
         , chop/2
         , between/3
        ]).

-export([
         odd/1
         , even/1
        ]).

% Common

return(Data) ->
  {return, Data}.

console(Data) ->
  console(Data, "~p~n").

console(Data, Format) ->
  io:format(Format, [Data]),
  {map, Data}.

% String

match(Data, Regex) ->
  case re:run(bucs:to_string(Data), bucs:to_string(Regex), [global]) of
    {match, _} ->
      {filter, true};
    _ ->
      {filter, false}
  end.

pad(Data, Size, Char) ->
  pad(Data, right, Size, Char).

pad(Data, right, Size, Char) ->
  bucs:as(Data, string:left(bucs:to_string(Data), Size, Char));
pad(Data, left, Size, Char) ->
  bucs:as(Data, string:right(bucs:to_string(Data), Size, Char)).

chop(Data, Size) ->
  bucs:as(Data, string:sub_string(bucs:to_string(Data), 1, Size)).

between(Data, Min, Max) ->
  {filter,
   bucs:to_string(Data) =< bucs:to_string(Max) andalso
   bucs:to_string(Data) >= bucs:to_string(Min)}.

% Integer

even(Data) ->
  {filter, Data rem 2 == 0}.

odd(Data) ->
  {filter, Data rem 2 /= 0}.

