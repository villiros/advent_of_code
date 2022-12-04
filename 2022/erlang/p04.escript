#!/usr/bin/env escript

read(FName) ->
    {ok, Lines} = file:read_file(FName),
    Toks = lists:map(fun erlang:list_to_integer/1, string:tokens(binary_to_list(Lines), "\n,-")),
    Parser =
        fun P([AL, AR, BL, BR | Rest]) ->
            [{{AL, AR}, {BL, BR}} | P(Rest)];
            P(_) -> []
        end,
    Parser(Toks).

intersect({AL, AR}, {BL, BR}) ->
    case {max(AL, BL), min(AR, BR)} of
        Res = {RL, RR} when RL =< RR -> Res;
        _ -> empty
    end.

main([FName]) ->
    Data = read(FName),

    % Part A
    PARows =
        [covers
             || {A, B} <- Data,
                R <- [intersect(A, B)],
                (R == A) orelse (R == B)],
    io:format("Part 1: ~p~n", [length(PARows)]),

    % Part B
    PBRows =
        [overlap
             || {A, B} <- Data,
                R <- [intersect(A, B)],
                R /= empty],
    io:format("Part 2: ~p~n", [length(PBRows)]),

    ok.