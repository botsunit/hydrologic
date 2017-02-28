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
        Pipeline = hydrologic:new([console]),
        Result = hydrologic:run(Pipeline, 10),
        ?assertEqual(10, Result),
        ?assertCall(io, format, 2, 1),
        hydrologic:stop(Pipeline)
    end,
    fun() ->
        Pipeline = hydrologic:new(
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
        ?assertEqual(20, hydrologic:run(Pipeline, 10)),
        hydrologic:stop(Pipeline)
    end
   ]}.
