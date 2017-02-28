-module(hydrologic_stdlib).

-export([
         return/1,
         console/1,
         console/2
        ]).

return(Data) ->
  {return, Data}.

console(Data) ->
  console(Data, "~p~n").

console(Data, Format) ->
  io:format(Format, [Data]),
  {map, Data}.
