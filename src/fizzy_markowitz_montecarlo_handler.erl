-module(fizzy_markowitz_montecarlo_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    io:format("Got Markowitz Monte Request ~p~n",[Req0]),
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    io:format("Got Markowitz Monte Body~p~n",[Data]),
    #{user:=User,
      user1:=User1,
      mean:=Mean,
      vol:=Vol,
      prem:=Prem,
      theta:=Theta,
      gamma:=Gamma,
      min:=Min,
      max:=Max,
      neff:=NEff,
      nprtf:=NPrtf,
      corr:=Corr} = jason:decode(Data,[{mode, map}]),

    Seed = rand:seed(exsplus, {42, 123534, 345345}),

    NC = length(User),
    NR = 5000,

    Rand0 = linalg:reshape(random(NR*NC,Seed),{NR,NC}),
    Tsum0 = [ lists:sum(Xs)/NR || Xs <- linalg:transpose(Rand0) ],
    io:format("Got sum0 ~p~n",[Tsum0]),
    Rand = linalg:sub(Rand0,lists:duplicate(NR,Tsum0)),
    Tsum = [ lists:sum(Xs)/NR || Xs <- linalg:transpose(Rand) ],
    io:format("Got sum0 ~p~n",[Tsum]),

    Chol = linalg:matmul(linalg:cholesky(Corr),linalg:diag(Vol)),

    TSDelta = linalg:matmul(Rand,Chol),
    TSGamma = linalg:mul(lists:duplicate(NR,linalg:mul(0.5,Gamma)),linalg:mul(TSDelta,TSDelta)),
    TSTheta = lists:duplicate(NR,Theta),
    TSGammaTheta = linalg:add(TSGamma,TSTheta),

    TSrow = linalg:add(linalg:add(TSDelta,TSGammaTheta),lists:duplicate(NR,Mean)),
    io:format("Got corr ~p~n",[Corr]),
    io:format("Got chol ~p~n",[Chol]),
    %io:format("Got data ~p~n",[TSrow]),
    %io:format("Got sum ~p~n",[TSGammaTheta]),

    MeanF = markowitz_montecarlo:meanf(TSrow),
    CoVarF = markowitz_montecarlo:variancef(TSrow),

    io:format("Got fun~p~n",[MeanF]),

    CoVar = linalg:matmul(Corr,linalg:diag(linalg:mul(Vol,Vol))),

    Candidates = markowitz_montecarlo:frontier(MeanF,CoVarF,{Min,Max},{NEff,NPrtf}),

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

    Req = cowboy_req:reply(200, #{ <<"content-type">> => <<"text/plain">> }, X, Req0),
    {ok, Req, State}.

random(N,Seed)->
  random(N,Seed,[]).

random(0,_,Acc)->
    Acc;
random(N,Seed,Acc)->
    {X,NextSeed}=rand:normal_s(Seed),
    random(N-1,NextSeed,[X|Acc]).

