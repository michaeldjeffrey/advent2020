-module(one).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    Computed = [{One, Two, One + Two} || One <- Input, Two <- Input],
    find_2020(Computed).

part2_solve(Input) ->
    Computed = [{One, Two, Three, One + Two + Three} || One <- Input, Two <- Input, Three <- Input],
    find_2020(Computed).

find_2020([]) -> bad_input;
find_2020([{One, Two, 2020} | _Rest]) -> One * Two;
find_2020([{One, Two, Three, 2020} | _Rest]) -> One * Two * Three;
find_2020([_H | Rest]) -> find_2020(Rest).

read_input(test) -> read_input("apps/one/src/test.txt");
read_input(main) -> read_input("apps/one/src/input.txt");
read_input(Filename) when is_list(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    [binary_to_integer(Line) || Line <- Lines].
