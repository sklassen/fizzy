%%%-------------------------------------------------------------------
%% @doc fizzy public API
%% @end
%%%-------------------------------------------------------------------

-module(fizzy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,Port} = application:get_env(fizzy,port),
    Dispatch = cowboy_router:compile([
            {'_', [
                {"/", fizzy_home_handler, []},
                {"/hello", fizzy_hello_handler, #{okay=>none}},
                {"/hello/:name", fizzy_hello_handler, #{okay=>ok}},
                {"/fizzy/markowitz/gaussian", fizzy_markowitz_gaussian_handler, []},
                {"/fizzy/markowitz/montecarlo", fizzy_markowitz_montecarlo_handler, []},
                {"/markowitz/gaussian", fizzy_markowitz_gaussian_handler, []},
                {"/markowitz/montecarlo", fizzy_markowitz_montecarlo_handler, []},
                {"/markowitz/brute", fizzy_markowitz_brute_handler, []},
                {"/[...]", cowboy_static, {priv_dir, fizzy, <<"www">>,[{mimetypes, cow_mimetypes, all}]}}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(fizzy_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}
    }),
    fizzy_sup:start_link().

stop(_State) ->
    ok.
