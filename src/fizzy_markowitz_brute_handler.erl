-module(fizzy_markowitz_brute_handler).
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

    MeanF = fun(Xs) -> markowitz_gaussian:mean(Mean,Xs) end,
    CoVarF = fun(Xs) -> markowitz_gaussian:variance(CoVar,Xs) end,

    Candidates = markowitz_brute:frontier(MeanF,CoVarF,{Min,Max},{NEff,NPrtf}),                                

    M0 = MeanF(User),
    V0 = CoVarF(User),

    M1 = MeanF(User1),
    V1 = CoVarF(User1),

    io:format("Got Markowitz user ~p~n",[User]),
    io:format("Got Markowitz user1 ~p~n",[User1]),
    %io:format("Got Markowitz mean ~p = ~p~n",[Mean,markowitz:mean(Mean,User)]),
    %io:format("Got Markowitz vol/corr ~p ~p~n",[Vol,Corr]),
    %io:format("Got Markowitz covar ~p = ~p~n",[CoVar,markowitz:variance(CoVar,User)]),

    %io:format("Got Markowitz candidates ~p~n",[Candidates]),
    Marshall = [ #{ mean=>M, vol=>V, weights=>Wgts, labels=>Labels} || {M,V,Wgts,Labels} <- lists:concat([Candidates,[{M0,V0,User,["user"]},{M1,V1,User1,["user"]}]]) ],

    %io:format("Got Markowitz candidates ~p~n",[Marshall]),

    X = jason:encode(#{candidates=>Marshall}),
    %io:format("Got Markowitz return ~p~n",[X]),

    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, X, Req1),
    {ok, Req2, State}.

