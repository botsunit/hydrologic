-module(hydrologic_stdlib_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

hydrologic_stdlib_test_() ->
  {setup,
   fun() ->
       meck:new(io, [passthrough, unstick]),
       meck:expect(io, format, fun(_Format, _Data) ->
                                   %?debugFmt(Format, Data)
                                   ok
                               end)
   end,
   fun(_) ->
       meck:unload(io)
   end,
   [
    fun() ->
        hydrologic:new(test, [console]),
        Result = hydrologic:run(test, 10),
        ?assertEqual({ok, 10}, Result),
        ?assertCall(io, format, 2, 1),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           fun(X) ->
               {map, X * 2}
           end,
           return,
           fun(X) -> %% Will never be called
               {map, X / 2}
           end
          ]
         ),
        ?assertEqual({ok, 20}, hydrologic:run(test, 10)),
        hydrologic:stop(test)
    end
   ]}.
