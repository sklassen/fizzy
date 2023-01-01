-module(fizzy_hello_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Fizzy Says, Hello World">>, Req0),
    {ok, Req, State}.

