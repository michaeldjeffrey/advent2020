-module(thirteen).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve({Target, BusIds}) ->
    BusTimes = lists:map(fun ({_I, BusId}) -> bus(BusId, 0, Target) end, BusIds),
    [{_MinutesToWait, Hash} | _] = lists:sort(BusTimes),
    Hash.

part2_solve({_Target, BusIds}) ->
    cr(BusIds).

%% https://github.com/lizthegrey/adventofcode/blob/main/2020/day13.go
%% Trevor!!! Explain
cr(Pairs) ->
    cr(Pairs, 0, 1).

cr([], Min, _) -> Min;
cr([{V, K} | Pairs], Min, Product) ->
    cr(Pairs,
       add_while_true(fun(X) -> (X+V) rem K =/= 0 end, Min, Product),
       Product * K).

add_while_true(Pred, One, Two) ->
    case Pred(One) of
        true -> add_while_true(Pred, One + Two, Two);
        false -> One
    end.

bus(Id, Current, Target) ->
    if
        Current < Target -> bus(Id, Current + Id, Target);
        true ->
            MinutesToWait = Current - Target,
            Hash = MinutesToWait * Id,
            {MinutesToWait, Hash}
    end.

read_input(test) -> read_input("apps/thirteen/src/test.txt");
read_input(main) -> read_input("apps/thirteen/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    [MinutesToWait, BusBin] = binary:split(Data, <<"\n">>, [global, trim]),
    BusIds = binary:split(BusBin, <<",">>,  [global]),
    IndexedBuses = lists:zip(lists:seq(0, length(BusIds)-1), BusIds),

    {binary_to_integer(MinutesToWait),
     to_integer_drop_x(IndexedBuses, [])}.

to_integer_drop_x([], Acc) -> Acc;
to_integer_drop_x([{_,<<"x">>}|Rest], Acc) ->
    to_integer_drop_x(Rest, Acc);
to_integer_drop_x([{I, Id}|Rest], Acc) ->
    to_integer_drop_x(Rest, [{I, binary_to_integer(Id)} | Acc]).
