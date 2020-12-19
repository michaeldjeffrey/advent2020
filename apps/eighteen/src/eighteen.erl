-module(eighteen).
-compile(export_all).
-export([]).

part1([], Acc) -> Acc;
part1([Last], Acc) ->
    case Last of
        {integer, _, Value} ->

            Acc(Value);
        {')', _} ->
            {ok, Acc, []};
        _ ->
            io:format("Something went wrong with ~p~n", [Last])
    end;
part1([Next | Rest], Acc) ->
    case Next of
        {integer, _, Value} ->
            part1(Rest, Acc(Value));
        {Sym, _} ->
            case Sym of
                '*' ->
                    part1(Rest, fun(N) -> Acc * N end);
                '+' ->
                    part1(Rest, fun(N) -> Acc + N end);
                '(' ->
                    {ok, Value, Left} = part1(Rest, fun identity/1),
                    part1([{integer, 1, Value} | Left], Acc);
                ')' ->
                    {ok, Acc, Rest}
            end
    end.

run1(S) ->
    {ok, I, _} = erl_scan:string(S),
    part1(I, fun identity/1).

run(S) ->
    {ok, I, _} = erl_scan:string(S),
    part2(I, []).


part2([], [{integer, _, Value}]) -> Value;

part2([], [{integer, _, Value}, {'(', _} | Tail]) -> {Value, Tail};

part2([], [{integer, _, One}, {'*', _}, {integer, _, Two} | Tail]) ->
    part2([], [{integer, 0, One*Two} | Tail]);

part2([Next | Rest], Stack) ->
    case {Next, Stack} of
        {{')', _}, _} ->
            {Value, Tail} = part2([], Stack),
            part2([{integer, 0, Value} | Rest], Tail);
        {{integer, _, Value}, [{'+', _}, {integer, _, Value2} | Tail]} ->
            part2(Rest, [{integer, 0, Value + Value2} | Tail]);
        _ ->
            part2(Rest, [Next | Stack])
    end.


identity(N) -> N.

test1() ->
    26 = run1("2 * 3 + (4 * 5)"),
    437 = run1("5 + (8 * 3 + 9 + 3 * 4 * 3)"),
    12240 = run1("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"),
    13632 = run1("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").

test2() ->
    51 = run("1 + (2 * 3) + (4 * (5 + 6))"),
    46 = run("2 * 3 + (4 * 5)"),
    1445 = run("5 + (8 * 3 + 9 + 3 * 4 * 3)"),
    669060 = run("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"),
    23340 = run("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").

main() ->
    Es = read_input(main),
    lists:sum([run(E) || E <- Es]).

read_input(main) -> read_input("src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    [binary_to_list(Line) || Line <- Lines].
