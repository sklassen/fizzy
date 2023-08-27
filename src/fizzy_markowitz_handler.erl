-module(fizzy_markowitz_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    io:format("Got Markowitz Request ~p~n",[Req0]),
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    io:format("Got Markowitz Body~p~n",[Data]),
    #{mean:=Mean,
      vol:=Vol,
      corr:=Corr} = jason:decode(Data,[{mode, map}]),

    CoVar = markowitz:cor2cov(Vol,Corr),

    Candidates = markowitz:candiates(Mean,CoVar),

    io:format("Got Markowitz candidates ~p~n",[Candidates]),
    Marshall = [ #{ mean=>M, vol=>V, weights=>Wgts, labels=>Labels} || {M,V,Wgts,Labels} <- Candidates  ],

    io:format("Got Markowitz candidates ~p~n",[Marshall]),

    X = jason:encode(#{candidates=>Marshall}),
    io:format("Got Markowitz return ~p~n",[X]),

    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, X, Req0),
    {ok, Req, State}.

