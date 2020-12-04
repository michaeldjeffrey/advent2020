-module(day2).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    Valid = lists:filter(fun ({entry, Min, Max, Needle, Password}) ->
                                 Count = count(Needle, Password),
                                 if
                                     Count > Max -> false;
                                     Count < Min -> false;
                                     true -> true
                                 end
                         end, Input),
    length(Valid).

part2_solve(Input) ->
    Valid = lists:filter(fun ({entry, First, Second, Needle, Password}) ->
                                 One = binary:at(Password, First-1),
                                 Two = binary:at(Password, Second-1),
                                 case {One, Two, Needle} of
                                     {Match, Match, Match} -> false;
                                     {Match, _, Match} -> true;
                                     {_, Match, Match} -> true;
                                     {_, _, _} -> false
                                 end
                         end, Input),
    length(Valid).

count(Needle, Hay) ->
    count(Needle, Hay, 0).

count(_Needle, <<>>, Count) ->
    Count;
count(Needle, <<Needle, Hay/binary>>, Count) ->
    count(Needle, Hay, Count + 1);
count(Needle, <<_Eat, Hay/binary>>, Count) ->
    count(Needle, Hay, Count).

line_to_entry(<<Min, $-, Max, $\s, Char, $:, $\s, Password/binary>>) ->
    {entry, binary_to_integer(<<Min>>), binary_to_integer(<<Max>>), Char, Password};
line_to_entry(<<MinOne, MinTwo, $-, MaxOne, MaxTwo, $\s, Char, $:, $\s, Password/binary>>) ->
    {entry, binary_to_integer(<<MinOne, MinTwo>>), binary_to_integer(<<MaxOne, MaxTwo>>), Char, Password};
line_to_entry(<<Min, $-, MaxOne, MaxTwo, $\s, Char, $:, $\s, Password/binary>>) ->
    {entry, binary_to_integer(<<Min>>), binary_to_integer(<<MaxOne, MaxTwo>>), Char, Password};
line_to_entry(<<MinOne, MinTwo, $-, Max, $\s, Char, $:, $\s, Password/binary>>) ->
    {entry, binary_to_integer(<<MinOne, MinTwo>>), binary_to_integer(<<Max>>), Char, Password}.

read_input(test) -> read_input("apps/day2/src/test.txt");
read_input(main) -> read_input("apps/day2/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    InputList = binary:split(Data, <<"\n">>, [global, trim]),
    lists:map(fun line_to_entry/1, InputList).
