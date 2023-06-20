-module(telegram_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [ {telegram,{telegram_srv,start,[]},permanent,8000,worker,dynamic} ],
	{ok, {{one_for_one, 10, 10}, Procs}}.
