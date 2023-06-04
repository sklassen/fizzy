%%%-------------------------------------------------------------------
%% @doc fizzy public API
%% @end
%%%-------------------------------------------------------------------

-module(fizzy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(fizzy,port),
    Dispatch = cowboy_router:compile([
            {'_', [
                {"/", fizzy_home_handler, []},
                {"/hello", fizzy_hello_handler, []}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(fizzy_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}
    }),
    fizzy_sup:start_link().

stop(_State) ->
    ok.
