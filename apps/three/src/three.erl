-module(three).

-export([part1/0, part2/0]).

-define(OPEN, $.).
-define(TREE, $#).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    count_with_run(3, Input).

part2_solve(Input) ->
    product([
             count_with_run(1, Input),
             count_with_run(3, Input),
             count_with_run(5, Input),
             count_with_run(7, Input),
             count_with_run(1, every_other_element(Input))
            ]).

count_with_run(Run, Input) ->
    Size = erlang:byte_size(hd(Input)),
    {_, Count} = lists:foldl(fun (Line, {X, Count}) ->
                                     case binary:at(Line, X rem Size) of
                                         ?OPEN -> {X + Run, Count};
                                         ?TREE -> {X + Run, Count + 1}
                                     end
                             end, {0, 0}, Input),
    Count.

every_other_element(List) ->
    IndexedInput = lists:zip(lists:seq(1, length(List)), List),
    [Line || {Index, Line} <- IndexedInput, Index rem 2 == 1].

product(List) ->
    Mul = fun(One, Two) -> One * Two end,
    lists:foldl(Mul, 1, List).

read_input(test) -> read_input("apps/three/src/test.txt");
read_input(main) -> read_input("apps/three/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    binary:split(Data, <<"\n">>, [global, trim]).
