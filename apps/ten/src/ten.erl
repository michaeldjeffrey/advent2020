-module(ten).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    MyDevice = lists:max(Input) + 3,
    Sorted = [0 | lists:sort(Input)],
    Zipped = lists:zipwith(fun erlang:'-'/2,
                           tl(Sorted) ++ [MyDevice],
                           Sorted),
    {Ones, Threes} = lists:partition(fun(X) -> X == 1 end, Zipped),
    length(Ones) * length(Threes).

part2_solve(Input) ->
    MyDevice = lists:max(Input) + 3,
    PathCache = lists:foldl(
                  fun(I, Cache) -> array:set(I, sum_of_3_window(I, Cache), Cache) end,
                  new_cache(MyDevice),
                  lists:sort(Input) ++ [MyDevice]),
    lists:max(array:to_list(PathCache)).

new_cache(Length) ->
    array:from_list([1 | lists:duplicate(Length, 0)]).

sum_of_3_window(I, Arr) ->
    lists:sum([safe_get(I-X, Arr) || X <- [1, 2, 3]]).

safe_get(I, _) when I < 0 -> 0;
safe_get(I, A) -> array:get(I, A).

read_input(test) -> read_input("apps/ten/src/test.txt");
read_input(main) -> read_input("apps/ten/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    List = binary:split(Data, <<"\n">>, [global, trim]),
    lists:map(fun binary_to_integer/1, List).
