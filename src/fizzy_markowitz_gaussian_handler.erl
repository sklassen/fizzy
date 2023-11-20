-module(fizzy_markowitz_gaussian_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    io:format("Got Markowitz Request ~p~n",[Req0]),
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    io:format("Got Markowitz Body~p~n",[Data]),
    #{user:=User,
      user1:=User1,
      mean:=Mean,
      vol:=Vol,
      min:=Min,
      max:=Max,
      neff:=NEff,
      nprtf:=NPrtf,
      corr:=Corr} = jason:decode(Data,[{mode, map}]),

    CoVar = linalg:matmul(Corr,linalg:diag(linalg:mul(Vol,Vol))),

    Candidates = markowitz_gaussian:frontier(Mean,CoVar,{Min,Max},{NEff,NPrtf}),

    M0 = markowitz_gaussian:mean(Mean,User),
    V0 = markowitz_gaussian:variance(CoVar,User),
    M1 = markowitz_gaussian:mean(Mean,User1),
    V1 = markowitz_gaussian:variance(CoVar,User1),

    io:format("Got Markowitz user ~p~n",[User]),
    %io:format("Got Markowitz mean ~p = ~p~n",[Mean,markowitz:mean(Mean,User)]),
    %io:format("Got Markowitz vol/corr ~p ~p~n",[Vol,Corr]),
    %io:format("Got Markowitz covar ~p = ~p~n",[CoVar,markowitz:variance(CoVar,User)]),

    %io:format("Got Markowitz candidates ~p~n",[Candidates]),
    Marshall = [ #{ mean=>M, vol=>V, weights=>Wgts, labels=>Labels} || {M,V,Wgts,Labels} <- lists:concat([Candidates,[{M0,V0,User,["user"]},{M1,V1,User1,["user"]}]]) ],

    %io:format("Got Markowitz candidates ~p~n",[Marshall]),

    X = jason:encode(#{candidates=>Marshall}),
    %io:format("Got Markowitz return ~p~n",[X]),

    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, X, Req0),
    {ok, Req, State}.

