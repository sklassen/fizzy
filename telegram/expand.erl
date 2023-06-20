-module(expand).
-export([split/1]).
%
% cartesian product, expansion or join
%
% cybot_expand:split("ABC[1,2,3,4,5]ABC").
% cybot_expand:split("ABC[1..5]ABC").
% cybot_expand:split("ABC[0..12,3]ABC").
% cybot_expand:split("LIBOR3M/[1y1y,5y5y]").

split(X) -> split([X], []).
split([], Acc) ->
  lists:reverse(Acc);
split([H | Tail], Acc) ->
  case re:run(H, "(.*)\\[(.*)\\](.*)", []) of
    {match, [_, Prefix, Body, Suffix]} ->
      split( lists:append(joinstr(substr(H, Prefix), substr(H, Body), substr(H, Suffix)), Tail), Acc);
    _ ->
      split(Tail, [H | Acc])
  end.

joinstr(Prefix, Expand, Suffix) ->
  [ string:join([Prefix,X,Suffix],"") || X <- sequence(Expand)].

splitstr(X) ->
  string:tokens(X, ",").

substr(X, {Start, End}) ->
  string:substr(X, Start + 1, End).

i(X)->
  string:to_integer(X).

sequence(X) ->
    case re:run( X, "([-+]?[0-9]*\\.?[0-9]+)\\.\\.([-+]?[0-9]*\\.?[0-9]+),?([-+]?[0-9]*\\.?[0-9]+)?", []) of
        {match, [_, Start, End, Incr]} ->
            seq( i(substr(X, Start)), i(substr(X, End)), i(substr(X, Incr)));
        {match, [_, Start, End]} ->
            seq(i(substr(X, Start)), i(substr(X, End)));
        _ ->
            splitstr(X)
    end.


seq({A, AA}, {B, BB}) ->
    case A > B of
        true -> seq({A, AA}, {B, BB}, {-1, []});
        false -> seq({A, AA}, {B, BB}, {1, []})
    end.

seq({A, []}, {B, []}, {I, []}) ->
    [integer_to_list(X) || X <- lists:seq(A, B, I)];
seq({A, AA}, {B, BB}, {I, II}) ->
    F = math:pow(10, max(max(length(AA), length(BB)), length(II)) - 1),
    [
        float_to_list(list_to_integer(X) / F, [{decimals, length(II)}, compact])
     || X <- seq(mul({A, AA}, F), mul({B, BB}, F), mul({I, II}, F))
    ].

mul({X, []}, N) ->
    {round(X * N), []};
mul({X, DP}, N) ->
    case string:to_float(integer_to_list(X) ++ DP) of
        {F, []} -> {round(F * N), []}
    end.
