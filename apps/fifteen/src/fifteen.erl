-module(fifteen).

-import(lists, [split/2, zip/2, seq/2]).
-export([part1/0, part2/0]).

part1() ->
    [{test, game(input(test), 2020)},
     {main, game(input(main), 2020)}].

part2() ->
    [{test, not_today},
     {main, game(input(main), 30000000)}].

game(Start, Target) ->
    Len = length(Start),
    {Input, [Last]} = split(Len-1, Start),
    Nums = maps:from_list(zip(Input, seq(1, Len-1))),
    game(Nums, Last, Len, Target).

game(_, Last, Target, Target) -> Last;
game(Nums, Last, Turn, Target) ->
    case maps:get(Last, Nums, badkey) of
        badkey ->
            game(Nums#{Last => Turn}, 0, Turn + 1, Target);
        TurnLastSpoken ->
            Next = Turn - TurnLastSpoken,
            game(Nums#{Last => Turn}, Next, Turn + 1, Target)
    end.

input(test) -> [0,3,6];
input(main) -> [17,1,3,16,19,0].
