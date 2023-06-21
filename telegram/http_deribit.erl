-module(http_deribit).
-export([start/0, start/1, reconnect/0, status/0, stop/0]).
-export([connect/0, verbose/1]).
-export([fetch/1, fetch_series/1]).
-include_lib("kernel/include/logger.hrl").

-define(ID, deribit).
-define(N, 1).
-define(TIMEOUT, 10000).

-record(state, {
    id = ?ID,
    npool = ?N,
    host = "forge",
    port = 8086,
    timeout = ?TIMEOUT,
    protocol,
    pid,
    message
}).

start([]) -> start().
start() ->
    {ok,Host} = application:get_env(fizzy, deribit_host),
    {ok,Port} = application:get_env(fizzy, deribit_port),
    case lists:member(?MODULE, registered()) of
        true ->
            {ok, whereis(?MODULE)};
        false ->
            register(?MODULE, spawn(fun() -> init(#state{host=Host,port=Port}) end)),
            {ok, whereis(?MODULE)}
    end.

init(State) ->
    loop(State).

open(#state{host = Host, port = Port}) ->
    case gun:open(Host, Port) of
        {ok, Pid} -> Pid;
        Error -> {error, Error}
    end.

reconnect() -> rpc(reconnect).
status() -> rpc(status).
stop() -> rpc(stop).

rpc(Q) ->
  case lists:member(?MODULE, registered()) of
    true ->
      ?MODULE ! {self(), Q},
      receive
        {?MODULE, Reply} -> Reply
      after 6000 -> timeout
      end;
    false ->
      stopped
  end.

loop(#state{pid = Pid, protocol = undefined} = State) when is_pid(Pid) ->
  case gun:await_up(Pid) of
    {ok, Protocol} ->
      loop(State#state{pid = Pid, protocol = Protocol});
    Error ->
      gun:close(Pid),
      wait(State#state{pid=undefined, message = Error})
  end;

loop(#state{pid = Pid} = State) ->
  receive
    {From, reconnect} ->
      case pg:get_members(node(),?ID) of
        Pool when length(Pool)>=?N ->
          From!{?MODULE,lists:nth(rand:uniform(length(Pool)),Pool)},
          loop(State);
        _Low  ->
          case open(State) of
            NewPid when is_pid(NewPid)->
              pg:join(node(),?ID,NewPid),
              From!{?MODULE, NewPid},
              loop(State#state{pid = NewPid, protocol = undefined});
            {error,Error}->
              From!{?MODULE, error},
              loop(State#state{message=Error})
          end
      end;
    {From, status} ->
      From ! {?MODULE, State},
      loop(State);
    {From, stop} ->
      From ! {?MODULE, stopped};
    {gun_up, Pid, http} ->
      loop(State);
    {gun_down, Pid, http, closed, []} ->
      loop(State);
    Unknown ->
      ?LOG_WARNING("(~p) UNKNOWN ~p", [?MODULE,Unknown]),
      loop(State)
  end.

wait(#state{timeout = TimeOut} = State) ->
  receive
    {From, reconnect} ->
      From ! {?MODULE, failed},
      wait(State);
    {From, status} ->
      From ! {?MODULE, State},
      wait(State);
    {From, stop} ->
      From ! {?MODULE, stopped};
    Unknown ->
      ?LOG_WARNING("(~p) UNKNOWN (disconnected) ~p", [?MODULE,Unknown]),
      wait(State)
  after TimeOut ->
          init(State#state{message = retry})
  end.

connect() ->
  case pg:get_members(node(),?ID) of
    Low when length(Low)<?N -> reconnect();
    Pool -> lists:nth(rand:uniform(length(Pool)), Pool)
  end.

verbose(N) ->
    case N of
        0 -> logger:set_module_level(?MODULE, error);
        1 -> logger:set_module_level(?MODULE, warning);
        2 -> logger:set_module_level(?MODULE, info);
        3 -> logger:set_module_level(?MODULE, debug);
        N -> {error, <<"verbose logger 0,1,2 or 3">>}
    end.

fetch(URL) ->
    %?LOG_ERROR("[mkt] deribit ~s~n",[URL]),
    %?LOG_ERROR("[mkt] deribit ~p~n",[uri_string:parse(URL)]),
    ConnPid = connect(),
    case uri_string:parse(URL) of
        #{scheme := "http", query := Query} ->
            StreamRef = gun:get(ConnPid, "/query?" ++ Query),
            ?LOG_INFO("http [~p] url ~p~n",[?ID,"/query?" ++ Query]),
            case gun:await(ConnPid, StreamRef, ?TIMEOUT) of
                {response, nofin, 200, _Headers} ->
                    case gun:await_body(ConnPid, StreamRef, ?TIMEOUT) of
                        {ok, Body} ->
                            %?LOG_ERROR("http [~p] body ~p~n",[?ID,Body]),
                            case jiffy:decode(Body) of

                                {[
                                    {<<"results">>, [
                                        {[
                                            {<<"statement_id">>, _},
                                            {<<"series">>, [
                                                {[
                                                    {<<"name">>, <<"rts">>},
                                                    {<<"tags">>, {[{<<"field">>,_}]}},
                                                    {<<"columns">>, [_, _]},
                                                    {<<"values">>, [[_Date, Number]]}
                                                ]}
                                            ]}
                                        ]}
                                    ]}
                                ]} ->
                                    to_number(Number);
                                {[
                                    {<<"results">>, [
                                        {[
                                            {<<"statement_id">>, _},
                                            {<<"series">>, [
                                                {[
                                                    {<<"name">>, <<"rts">>},
                                                    {<<"columns">>, [<<"time">>, <<"last">>]},
                                                    {<<"values">>, [[_Date, Number]]}
                                                ]}
                                            ]}
                                        ]}
                                    ]}
                                ]} ->
                                    to_number(Number);
                                {[
                                    {<<"results">>, [
                                        {[
                                            {<<"statement_id">>, _},
                                            {<<"series">>, [
                                                {[
                                                    {<<"name">>, <<"rts">>},
                                                    {<<"tags">>, {[{<<"field">>, <<"ASK">>}]}},
                                                    {<<"columns">>, [<<"time">>, <<"last">>]},
                                                    {<<"values">>, [[_DateA, Ask]]}
                                                ]},
                                                {[
                                                    {<<"name">>, <<"rts">>},
                                                    {<<"tags">>, {[{<<"field">>, <<"BID">>}]}},
                                                    {<<"columns">>, [<<"time">>, <<"last">>]},
                                                    {<<"values">>, [[_DateB, Bid]]}
                                                ]},
                                                {[
                                                    {<<"name">>, <<"rts">>},
                                                    {<<"tags">>, {[{<<"field">>, <<"LAST_PRICE">>}]}},
                                                    {<<"columns">>, [<<"time">>, <<"last">>]},
                                                    {<<"values">>, [[_DateL, LastPrice]]}
                                                ]},
                                                {[
                                                    {<<"name">>, <<"rts">>},
                                                    {<<"tags">>, {[{<<"field">>, <<"MID">>}]}},
                                                    {<<"columns">>, [<<"time">>, <<"last">>]},
                                                    {<<"values">>, [[_DateM, Mid]]}
                                                ]}
                                            ]}
                                        ]}
                                    ]}
                                ]} ->

                                    % if is_number(Bid) andalso is_number(Ask) andalso is_number(Mid) ->
                                    %     if (Bid =< Mid) andalso (Mid =< Ask) ->
                                    %         Mid;
                                    %     true ->
                                    %         to_number(LastPrice);
                                    % true ->
                                    %     to_number(LastPrice);

                                    % case Mid of
                                    %     _ when is_number(Mid) andalso is_number(Bid) andalso is_number(Ask) andalso (Bid =< Mid) andalso (Mid =< Ask) ->
                                    %         Mid;
                                    %     _ when is_number(LastPrice) ->
                                    %         LastPrice;
                                    %     _ ->
                                    %         erlang:error("Can't parse price from Response (" ++ Body ++ ")")
                                    % end;

                                    choose_price(Bid, Mid, Ask, LastPrice);
                                _ ->
                                    erlang:error({error, "Can't parse URL (" ++ URL ++ ")"})
                            end;
                        {error, timeout} ->
                            ?LOG_ERROR("http [~p] timeout body (~p) ~p line ~p",[?ID,URL,?FILE,?LINE]),
                            erlang:error({error, "timeout: " ++ URL});
                        Unknown ->
                            ?LOG_ERROR("http [~p] unknown (~p) ~p ~p line ~p",[?ID,Unknown,URL,?FILE,?LINE]),
                            erlang:error({error, "Can't load URL (" ++ URL ++ ")"})
                    end;
                {error, timeout} ->
                    ?LOG_ERROR("http [~p] timeout request (~p) ~p line ~p",[?ID,URL,?FILE,?LINE]),
                    erlang:error({error, "timeout: " ++ URL});
                Bad ->
                    ?LOG_ERROR("http [~p] bad (~p) ~p line ~p",[?ID,Bad,?FILE,?LINE]),
                    erlang:error({error, "Connecttion error (not 200) (" ++ URL ++ ")"})
            end;
        NoConnection ->
            ?LOG_ERROR("http [~p] bad (~p) ~p line ~p",[?ID,NoConnection,?FILE,?LINE]),
            erlang:error({error, "Can't load unknown scheme (" ++ URL ++ ")"})
    end.

choose_price(Bid, Mid, Ask, _LastPrice) when is_number(Mid), is_number(Bid), is_number(Ask), (Bid =< Mid), (Mid =< Ask) ->
    Mid;
choose_price(_Bid, _Mid, _Ask, LastPrice) when is_number(LastPrice) ->
    LastPrice;
choose_price(_, _, _, _) ->
    erlang:error("Can't parse price from Response").


fetch_series(URL) ->
    %?LOG_DEBUG("[mkt] deribit ~s~n",[URL]),
    %?LOG_DEBUG("[mkt] deribit ~p~n",[uri_string:parse(URL)]),
    case uri_string:parse(URL) of
        #{scheme := "http", query := Query} ->
            ConnPid = connect(),
            StreamRef = gun:get(ConnPid, "/query?" ++ Query),
            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _Headers} ->
                    case gun:await_body(ConnPid, StreamRef, ?TIMEOUT) of
                        {ok, Body} ->
                            ?LOG_DEBUG("[mkt] deribit ~p~n",[Body]),
                            case jiffy:decode(Body) of
                                {[
                                    {<<"results">>, [
                                        {[
                                            {<<"statement_id">>, _},
                                            {<<"series">>, Series }
                                        ]}
                                    ]}
                                ]} -> Series;

                                _ ->
                                    erlang:error({error, "Can't parse URL (" ++ URL ++ ")"})
                            end;
                        {error, timeout} ->
                            erlang:error({error, "timeout: " ++ URL});
                        _ ->
                            erlang:error({error, "Can't load URL (" ++ URL ++ ")"})
                    end;
                Error ->
                    ?LOG_ERROR("Connecttion error ~p (not 200) ~p (~p line ~p)",[Error,URL,?FILE,?LINE]),
                    erlang:error({error, "Connecttion error (not 200) (" ++ URL ++ ")"})
            end;
        _ ->
            erlang:error({error, "Can't load unknown scheme (" ++ URL ++ ")"})
    end.

to_number(X) when is_number(X) ->
  X;
to_number(X) ->
    case string:to_float(X) of
        {Float, <<>>} ->
            Float;
        {Float, []} ->
            Float;
        {error, no_float} ->
            case string:to_integer(X) of
                {Integer, _} -> Integer;
                _Error -> error
            end
    end.

