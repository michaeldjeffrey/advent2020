-module(five).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, data_not_applicable_to_part},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    lists:max([ID || {seat, {_, _, ID}} <- lists:map(fun seat/1, Input)]).

part2_solve(Input) ->
    Seating = lists:map(fun seat/1, Input),
    ExistingIds = sets:from_list([ID || {seat, {_, _, ID}} <- Seating]),
    Ids = [(Row *8) + Col || Row <- lists:seq(10, 117), Col <- lists:seq(0, 7)],
    [SeatNum] = lists:filter(fun(Id) ->
                                     not sets:is_element(Id, ExistingIds) andalso
                                         sets:is_element(Id-1, ExistingIds) andalso
                                         sets:is_element(Id+1, ExistingIds)
                             end, Ids),
    SeatNum.

seat(Bin) ->
    <<Row:7/binary, Column/binary>> = Bin,
    RowNum = search_for(Row, {0, 127}, {$F, $B}),
    ColNum = search_for(Column, {0, 7}, {$L, $R}),
    {seat, {RowNum, ColNum, (RowNum *8) + ColNum}}.

search_for(<<>>, {Low, _}, _) -> Low;
search_for(Bin, {Low, High}, {Left, Right} = LR) ->
    Mid = (Low + High) div 2,
    case Bin of
        <<Left, Rest/binary>> -> search_for(Rest, {Low, Mid-1}, LR);
        <<Right, Rest/binary>> -> search_for(Rest, {Mid+1, High}, LR)
    end.

read_input(test) -> read_input("apps/five/src/test.txt");
read_input(main) -> read_input("apps/five/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    binary:split(Data, <<"\n">>, [global, trim]).
