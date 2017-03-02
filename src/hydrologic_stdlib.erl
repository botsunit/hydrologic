-module(hydrologic_stdlib).

-export([
         return/1
         , console/1
         , console/2
         , match/2
         , pad/3
         , pad/4
         , chop/2
        ]).

return(Data) ->
  {return, Data}.

console(Data) ->
  console(Data, "~p~n").

console(Data, Format) ->
  io:format(Format, [Data]),
  {map, Data}.

match(Data, Regex) ->
  case re:run(bucs:to_string(Data), bucs:to_string(Regex), [global]) of
    {match, _} ->
      {reduce, true};
    _ ->
      {reduce, false}
  end.

pad(Data, Size, Char) ->
  pad(Data, right, Size, Char).

pad(Data, right, Size, Char) ->
  bucs:as(Data, string:left(bucs:to_string(Data), Size, Char));
pad(Data, left, Size, Char) ->
  bucs:as(Data, string:right(bucs:to_string(Data), Size, Char)).

chop(Data, Size) ->
  bucs:as(Data, string:sub_string(bucs:to_string(Data), 1, Size)).

