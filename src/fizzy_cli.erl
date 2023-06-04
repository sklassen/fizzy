-module(fizzy_cli).
-export([hello/1]).

hello(Text) ->
  {ok,Port} = application:get_env(fizzy,port),
  {ok,ConnPid} = gun:open({127,0,0,1}, Port),

  StreamRef = gun:get(ConnPid, "/"),
  case gun:await(ConnPid, StreamRef) of
    {response, fin, Status, Headers} ->
      no_data;
    {response, nofin, Status, Headers} ->
      {ok, Body} = gun:await_body(ConnPid, StreamRef),
      Body
  end.
