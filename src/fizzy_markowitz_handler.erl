-module(fizzy_markowitz_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    io:format("Got Markowitz Request ~p~n",[Req0]),
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    io:format("Got Markowitz Body~p~n",[Data]),
    #{user:=User,
      mean:=Mean,
      vol:=Vol,
      corr:=Corr} = jason:decode(Data,[{mode, map}]),

    CoVar = markowitz:cor2cov(Vol,Corr),

    M0 = markowitz:mean(Mean,User),
    V0 = markowitz:variance(CoVar,User),

    io:format("Got Markowitz user ~p~n",[User]),
    io:format("Got Markowitz mean ~p = ~p~n",[Mean,markowitz:mean(Mean,User)]),
    io:format("Got Markowitz vol/corr ~p ~p~n",[Vol,Corr]),
    io:format("Got Markowitz covar ~p = ~p~n",[CoVar,markowitz:variance(CoVar,User)]),

    Candidates = markowitz:frontier(Mean,CoVar),

    io:format("Got Markowitz candidates ~p~n",[Candidates]),
    Marshall = [ #{ mean=>M, vol=>V, weights=>Wgts, labels=>Labels} || {M,V,Wgts,Labels} <- lists:concat([Candidates,[{M0,V0,User,["user"]}]]) ],

    io:format("Got Markowitz candidates ~p~n",[Marshall]),

    X = jason:encode(#{candidates=>Marshall}),
    io:format("Got Markowitz return ~p~n",[X]),

    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, X, Req0),
    {ok, Req, State}.

