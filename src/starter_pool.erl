-module(starter_pool).

-export([evaluate/2, evaluate/3]).

evaluate(Lng, Code) ->
    evaluate(Lng, Code, []).
evaluate(Lng, Code, InputFiles) ->
    F = fun(Worker) ->
            gen_server:call(Worker, {evaluate, Lng, Code, InputFiles})
        end,
    try poolboy:transaction(starter, F)
    catch
        exit:Reason -> {error, Reason}
    end.
