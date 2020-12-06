-module(day6).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part1_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    AnyoneAnsweredYes =
        map(
          pipe([
                fun binary_to_list/1,
                fun sets:from_list/1,
                fun (X) -> sets:del_element($\n, X) end,
                fun sets:size/1
               ])),
    lists:sum(AnyoneAnsweredYes(Input)).

part2_solve(Input) ->
    EveryoneAnsweredYes =
        map(
          pipe([
                fun (X) -> binary:split(X, <<"\n">>, [global, trim]) end,
                map(fun binary_to_list/1),
                map(fun sets:from_list/1),
                fun sets:intersection/1,
                fun sets:size/1
               ])),
    lists:sum(EveryoneAnsweredYes(Input)).

map(Fn) ->
    fun (X) ->
            lists:map(Fn, X)
    end.

pipe(Fns) ->
    fun (Input) ->
            pipe(Fns, Input)
    end.

pipe([], Res) -> Res;
pipe([Fn | Fns], Res) ->
    pipe(Fns, Fn(Res)).

read_input(test) -> read_input("apps/day6/src/test.txt");
read_input(main) -> read_input("apps/day6/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    binary:split(Data, <<"\n\n">>, [global, trim]).
