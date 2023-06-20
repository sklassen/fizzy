-module(telegram_srv).

-export([start/0,stop/0]).

-define(TOKEN, "163561540:AAEAeeTSpQaHj5qrfgMyhfFzlUw2bC7jOI8").
-define(BASE_URL, "https://api.telegram.org/bot" ++ ?TOKEN).
-define(GET_COMMAND_URL, ?BASE_URL ++ "/getUpdates?offset=").
-define(SET_COMMAND_URL, ?BASE_URL ++ "/sendMessage").


start() ->
	inets:start(),
    case lists:member(telegram,registered()) of
        true -> {ok,whereis(telegram)};
        false -> register(telegram, spawn_link(fun() -> init() end)), {ok,whereis(telegram)}
    end.

init() ->
        io:format("---Start bot---~n"),
        inets:start(),
        ssl:start(),
        loop(0).

rpc(Q) ->
    case lists:member(telegram,registered()) of
        true -> telegram!{self(), Q},
                receive
                    {telegram, Reply} -> Reply
                end;
        false -> inactive
    end.

stop() -> rpc(stop).

loop(State) ->
	NewState=poll(State),
    receive
        {From, stop} -> From!{telegram, stopped};
		_ -> io:format("~w~n", [error]),loop(NewState)
    after
        3000 -> loop(NewState)
    end.



poll(UpdateId) ->
        Response = parse_response(http_get(?GET_COMMAND_URL ++ integer_to_list(UpdateId + 1))),
        {JsonObj} = jiffy:decode(Response),
        Result = proplists:get_value(<<"result">>, JsonObj, []),
		%io:put_chars(standard_error,io_lib:format("out:~p",[Result])),
        case Result of
                [{[{<<"update_id">>, NewUpdateId}, {<<"message">>, {Message}}]}|_] -> 
                        {From} = proplists:get_value(<<"from">>, Message),
                        Username = proplists:get_value(<<"username">>, From),
                        ChatID = proplists:get_value(<<"id">>, From),
                        Command = proplists:get_value(<<"text">>, Message),
						case binary_to_list(Command) of 
							"/plead" -> send_message(ChatID,telegram:plead(Username));
							General when General=="/start" orelse General=="/help"  orelse General=="hi" orelse General=="hello" orelse General=="Hi" orelse General=="Hello"
										->	send_message(ChatID,telegram:reply(General));
							Admin when Admin=="/pending" orelse  Admin=="/grant" orelse  Admin=="/blacklist"
							              -> case telegram:is_admin(Username) of
												true -> send_message(ChatID,telegram:admin(Admin));
												false -> send_message(ChatID, "<Admin command only>")
											end;
							Text -> 		case telegram:is_authorized(Username) of
												true -> send_message(ChatID,telegram:reply(Text));
												false -> send_message(ChatID, "<Unauthorized>")
									   		end
						end,
						NewUpdateId;
                [] -> 
                        %io:format("~w~n", [empty]),
                        UpdateId;
				Unknown -> io:put_chars(standard_error,io_lib:format("unknown package:~p",[Unknown]))
        end.

send_message(ChatID, Text) ->
	io:put_chars(standard_error,io_lib:format("send:~p~n",[ChatID])),
	http_post(?SET_COMMAND_URL, "chat_id=" ++ integer_to_list(ChatID) ++ "&text=" ++ Text).

http_get(Url) ->
        http_request(get, {Url, []}).

http_post(Url, Data) -> 
        Response = http_request(post, {Url, [], "application/x-www-form-urlencoded", Data}),
        {ok, {{"HTTP/1.1",ReturnCode, State}, _Head, _Body}} = Response,
        io:format("~w / ~w~n", [ReturnCode, State]).

http_request(Method, Body) ->
    httpc:request(Method, Body, [{ssl,[{verify,0}]}], []).

parse_response({ok, { _, _, Body}}) ->
         Body.

