-module(arithmetic).
-export([is_arithmetic/1, eval/1, to_erlang/1]).
-export([thirtyseconds/1, sixtyfourths/1]).

% gwei is ETH base units

is_arithmetic([]) ->
    false;
is_arithmetic(X) ->
    match ==
        re:run(
            X,
            "^([0-9\sF()\"'+-/*,%\\$]|bp|c|cent|cents|k|zen|qian|ki|man|wan|mm|mn|man|lakh|cr|crore|bn|bio|tr|trl|gwei|oku|yi|chou|sqrt|log|pow|exp|sin|cos|tan|atanh)+$",
            [{capture, none}]
        ).

eval(Text) ->
    case eval(cybot_arithmetic:to_erlang(string:trim(Text)), []) of
        {value, N, _} when is_float(N) -> round(N * 10000000) / 10000000;
        {value, N, _} -> N
    end.

eval(S, Environ) ->
    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed, Environ).

to_erlang(Text) ->
    lists:foldl(fun(F, Acc) -> to_erlang(F, Acc) end, Text, [
        space,
        plus,
        divide,
        dblminus,
        comma,
        sixtyfourths,
        thirtyseconds,
        func,
        crore,
        cent,
        percent,
        bp,
        tr,
        bn,
        lakh,
        man,
        oku,
        gwei,
        mm,
        cent,
        k
    ]) ++ ".".

to_erlang(space, X) ->
    re:replace(X, " ", "", [{return, list}, global]);
to_erlang(plus, X) ->
    re:replace(X, "%2B", "+", [{return, list}, global]);
to_erlang(divide, X) ->
    re:replace(X, "%2F", "/", [{return, list}, global]);
to_erlang(dblminus, X) ->
    re:replace(X, "--", "+", [{return, list}, global]);
to_erlang(comma, X) ->
    re:replace(X, ",", "", [{return, list}, global]);
to_erlang(thirtyseconds, X) ->
    re:replace(
        X, "'([-]?)([0-9]+)-([0-9]+)([+]?)", "cybot_arithmetic:thirtyseconds(\"\\1\\2-\\3\\4\")", [
            {return, list},
            global
        ]
    );
to_erlang(sixtyfourths, X) ->
    re:replace(
        X, "\"([-]?)([0-9]+)-([0-9]+)([+]?)", "cybot_arithmetic:sixtyfourths(\"\\1\\2-\\3\\4\")", [
            {return, list},
            global
        ]
    );
to_erlang(func, X) ->
    re:replace(X, "(log|sqrt|exp|sin|cos|tan|atanh)", "math:\\1", [{return, list}, global]);
to_erlang(percent, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)%", "(\\1/100)", [{return, list}, global]);
to_erlang(bp, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)bp", "(\\1/10000)", [{return, list}, global]);
to_erlang(gwei, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(gwei)", "(\\1/1000000000)", [{return, list}, global]);
to_erlang(k, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(k|zen|qian|ki)", "(\\1*1000)", [{return, list}, global]);
to_erlang(man, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(man|wan)", "(\\1*10000)", [{return, list}, global]);
to_erlang(lakh, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)lakh", "(\\1*100000)", [{return, list}, global]);
to_erlang(mm, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(mm|mn)", "(\\1*1000000)", [{return, list}, global]);
to_erlang(crore, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(crore|cr)", "(\\1*10000000)", [{return, list}, global]);
to_erlang(bn, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(bn|bio)", "(\\1*1000000000)", [{return, list}, global]);
to_erlang(tr, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(tr|trl|chou)", "(\\1*1000000000000)", [ {return, list}, global ]);
to_erlang(oku, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(oku|yi)", "(\\1*100000000)", [{return, list}, global]);
to_erlang(cent, X) ->
    re:replace(X, "(-?[0-9]+\\.?[0-9]*)(c|cent|cents)", "(\\1/100)", [{return, list}, global]);
to_erlang(_, X) ->
    X.

thirtyseconds(X) ->
    case re:run(X, "([-]?)([0-9]+)-([0-9]+)([+]?)", [{capture, [1, 2, 3, 4], list}]) of
        {match, [[], Int, Frac, []]} ->
            element(1, string:to_integer(Int)) + element(1, string:to_integer(Frac)) / 32;
        {match, [[], Int, Frac, "+"]} ->
            element(1, string:to_integer(Int)) + (element(1, string:to_integer(Frac)) + 0.5) / 32;
        {match, ["-", Int, Frac, []]} ->
            -1 * element(1, string:to_integer(Int)) - element(1, string:to_integer(Frac)) / 32;
        {match, ["-", Int, Frac, "+"]} ->
            -1 * element(1, string:to_integer(Int)) -
                (element(1, string:to_integer(Frac)) + 0.5) / 32;
        nomatch ->
            {error, not_32nds}
    end.

sixtyfourths(X) ->
    case re:run(X, "([-]?)([0-9]+)-([0-9]+)([+]?)", [{capture, [1, 2, 3, 4], list}]) of
        {match, [[], Int, Frac, []]} ->
            element(1, string:to_integer(Int)) + element(1, string:to_integer(Frac)) / 64;
        {match, [[], Int, Frac, "+"]} ->
            element(1, string:to_integer(Int)) + (element(1, string:to_integer(Frac)) + 0.5) / 64;
        {match, ["-", Int, Frac, []]} ->
            -1 * element(1, string:to_integer(Int)) - element(1, string:to_integer(Frac)) / 64;
        {match, ["-", Int, Frac, "+"]} ->
            -1 * element(1, string:to_integer(Int)) -
                (element(1, string:to_integer(Frac)) + 0.5) / 64;
        nomatch ->
            {error, not_32nds}
    end.
