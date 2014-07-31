-module(evaluate_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
    {Method, Req2} = cowboy_req:method(Req),
    init_by_method(Method, Req2).

% only post requests allowed
init_by_method(<<"POST">>, Req) ->
    {ok, Req, undefined_state};
init_by_method(Method, Req) ->
    Response = {<<"error">>, list_to_binary("Incorrect method used: " ++ atom_to_list(Method) ++ ".")},
    {ok, Req2} = cowboy_req:reply(400, [
        {<<"content-type">>, <<"application/json">>}
    ], jsx:encode(Response), Req),
    {shutdown, Req2, undefined_state}.

% FIXME only post request
handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Params = jsx:decode(Body),
    Lang = proplists:get_value(<<"language">>, Params),
    Code = proplists:get_value(<<"code">>, Params),
    % lager:debug(Code),
    Response = case starter_pool:evaluate(Lang, Code) of
        {ok, Result} -> Result;
        {error, Result} -> Result
    end,

    {ok, Req3} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
    ], jsx:encode(Response), Req2),

    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
