-module(day9).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test), 5)},
     {main, part1_solve(read_input(main), 25)}].

part2() ->
    [{test, part2_solve(read_input(test), 127)},
     {main, part2_solve(read_input(main), 29221323)}].

part1_solve(Input, PreambleSize) ->
    {Preamble, [Number | Rest]} = lists:split(PreambleSize, Input),
    find_first_invalid(Preamble, Number, Rest).

part2_solve(Input, Target) ->
    ReducedInput = lists:takewhile(fun(X) -> X /= Target end, Input),
    Range = find_contigious_sum_backwards(ReducedInput, Target),
    Min = lists:min(Range),
    Max = lists:max(Range),
    Min + Max.

find_first_invalid(Preamble, Candidate, Input) ->
    case valid(Preamble, Candidate) of
        false ->
            Candidate;
        true ->
            [_ | NextPreamble] = Preamble,
            [NextCandidate | Rest] = Input,
            find_first_invalid(
              lists:append(NextPreamble, [Candidate]),
              NextCandidate,
              Rest)
    end.

find_contigious_sum_backwards(Input, Target) ->
    Size = length(Input),
    find_contigious_sum_backwards(Input, Size-2, Size, Target).

find_contigious_sum_backwards(Input, WindowStart, WindowEnd, Target) ->
    Search = fun Recur(Start, End) ->
                     Range = lists:sublist(Input, Start, End - Start),
                     Sum = lists:sum(Range),
                     case compare(Sum, Target) of
                         equal -> Range;
                         lesser -> Recur(Start - 1, End);
                         greater -> Recur(End - 3, End - 1)
                     end
             end,
    Search(WindowStart, WindowEnd).

valid(Preamble, Number) ->
    Combos = [X + Y || X <- Preamble, Y <- Preamble, X /= Y],
    lists:member(Number, Combos).

compare(X, Y) when X == Y -> equal;
compare(X, Y) when X < Y -> lesser;
compare(X, Y) when X > Y -> greater.

read_input(test) -> read_input("apps/day9/src/test.txt");
read_input(main) -> read_input("apps/day9/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    List = binary:split(Data, <<"\n">>, [global, trim]),
    lists:map(fun binary_to_integer/1, List).
