-module(fizzy_home_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Fizzy Home">>, Req0),
    {ok, Req, State}.

