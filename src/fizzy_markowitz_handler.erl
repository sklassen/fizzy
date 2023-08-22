-module(fizzy_markowitz_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(#{bindings := #{name := Name}}=Req0, State) ->
    io:format("Got Request ~p~n",[Req0]),
    io:format("Got State ~p~n",[State]),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Fizzy Says, Hello ",Name/binary>>, Req0),
    {ok, Req, State};

init(Req0, State) ->
    io:format("Got Request ~p~n",[Req0]),
    io:format("Got State ~p~n",[State]),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Fizzy Says, Hello Unknown">>, Req0),
    {ok, Req, State}.

