-module(telegram).
-import(lists,[member/2,sort/1,umerge/1]).
-export([is_admin/1,is_authorized/1,plead/1,admin/1,reply/1]).

-define(CFG, "/home/telegram/telegram.conf").


is_admin([undefined])-> false;
is_admin(X) when is_binary(X)->is_admin(binary_to_list(X));
is_admin(Who)->
	io:put_chars(standard_error,io_lib:format("who:~p~n",[Who])),
	{ok,[{admin,Admin},{users,_},{pending,_},{blacklist,_}]}=file:consult(?CFG),
	member(Who,Admin).

is_authorized([undefined])-> false;
is_authorized(X) when is_binary(X)->is_authorized(binary_to_list(X));
is_authorized(Who)->
	io:put_chars(standard_error,io_lib:format("who:~p~n",[Who])),
	{ok,[{admin,Admin},{users,Users},{pending,_},{blacklist,_}]}=file:consult(?CFG),
	member(Who,Admin) orelse member(Who,Users).

% Responses
plead(undefined)-> "<set user name>";
plead(X) when is_binary(X)->plead(binary_to_list(X));
plead(Who) when is_list(Who) ->
	io:put_chars(standard_error,io_lib:format("plead:~p~n",[Who])),
	{ok,[{admin,Admin},{users,Users},{pending,Pending},{blacklist,Blacklist}]}=file:consult(?CFG),
	case {member(Who,Users),member(Who,Pending),member(Who,Blacklist)} of
		{true,_,_} -> "<Already Member>";
		{_,true,_} -> "<Pending Member>";
		{_,_,true} -> "<Rejected>";
		{_,_,_} -> save(Admin,Users,[Who|Pending],Blacklist),"<Application pending approval>"
	end.
	

admin("/pending")->
	{ok,[{admin,_},{users,_},{pending,Pending},{blacklist,_}]}=file:consult(?CFG),
	case Pending of
		[] -> "<None Pending>";
		Pending -> "<Pending: "++string:join(Pending,",")++" >"
	end;
	
admin("/grant")->
	{ok,[{admin,Admin},{users,Users},{pending,Pending},{blacklist,Blacklist}]}=file:consult(?CFG),
	case Pending of
		[] -> "<None Pending>";
		Pending -> save(Admin,umerge([sort(Pending),sort(Users)]),[],Blacklist),"<Granted: "++string:join(Pending,",")++" >"
	end;

admin("/blacklist")->
	{ok,[{admin,Admin},{users,Users},{pending,Pending},{blacklist,Blacklist}]}=file:consult(?CFG),
	case Pending of
		[] -> "<None Pending>";
		Pending -> save(Admin,Users,[],umerge([sort(Pending),sort(Blacklist)])),"<Blacklisted: "++string:join(Pending,",")++" >"
	end;
admin(X)->"<Unknown Command "++X++">".

% Responses
reply(Test) when Test=="Test" orelse Test=="test" -> 
	"Test message from cybot, hello";

reply(Hello) when Hello=="Hello" orelse Hello=="hello" orelse Hello=="Hi" orelse Hello=="hi" -> 
	"Cybot says hello";

reply("/start") -> 
	"Cybot says welcome";

reply("/help") -> 
	"Help is on it's way. In the meantime, please read the cybot short guide.";

reply(Msg) ->
		case httpc:request("http://localhost/cybot/"++http_uri:encode(string:to_lower(Msg))++"?format=txt") of
			{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> Body;
			_ -> "Error: cybot connection"
		end.

% consult file
save(Admin,Users,Pending,Blacklist)->
	ok = file:write_file(?CFG, io_lib:format("{admin,~p}.~n{users,~p}.~n{pending,~p}.~n{blacklist,~p}.~n", [Admin,Users,Pending,Blacklist])).
