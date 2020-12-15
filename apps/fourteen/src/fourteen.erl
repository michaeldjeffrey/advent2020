-module(fourteen).

-export([part1/0, part2/0, computer_v1/2, computer_v2/2]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test2))},
     {main, part2_solve(read_input(main))}].


part1_solve(Input) ->
    sum_from_computer(new_computer(), Input).

part2_solve(Input) ->
    sum_from_computer(new_computer_v2(), Input).

sum_from_computer(Computer, Input) ->
    ok = send_instructions(Computer, Input),
    Computer ! {sum, self()},
    Sum = receive Msg -> Msg end,
    Computer ! stop,
    Sum.

send_instructions(_Pid, []) ->
    ok;
send_instructions(Pid, [I | Is]) ->
    Pid ! I,
    send_instructions(Pid, Is).

new_computer() ->
    spawn_link(?MODULE, computer_v1, [undefined, #{}]).

new_computer_v2() ->
    spawn_link(?MODULE, computer_v2, [new_computer(), #{}]).

computer_v1(Mask, Memory) ->
    receive
        {mask, Mask1} ->
            computer_v1(Mask1, Memory);
        {memset, Location, Value} ->
            computer_v1(Mask, Memory#{Location => apply_mask(Mask, Value)});
        {memsetv2, Location, Value} ->
            computer_v1(Mask, Memory#{Location => Value});
        {memory, From} ->
            From ! Memory,
            computer_v1(Mask, Memory);
        {sum, From} ->
            Sum = lists:sum(maps:values(Memory)),
            From ! Sum,
            computer_v1(Mask, Memory);
        stop -> ok
    end.

computer_v2(Mask, Computer) ->
    receive
        {mask, Mask1} ->
            computer_v2(Mask1, Computer);
        {memset, Location, Value} ->
            send_instructions(
              Computer,
              [{memset_v2, L, Value} || L <- permute_mask(Mask, Location)]),
            computer_v2(Mask, Computer);
        stop ->
            Computer ! stop,
            ok;
        Msg ->
            Computer ! Msg,
            computer_v2(Mask, Computer)
    end.

base2(Num) ->
    Num1 = erlang:integer_to_list(Num, 2),
    lists:flatten(lists:duplicate(36 - length(Num1), "0") ++ Num1).

unbase2(L) ->
    erlang:list_to_integer(lists:reverse(L), 2).

apply_mask(Mask, Value)   -> apply_mask(Mask, base2(Value), []).
permute_mask(Mask, Value) -> permute_mask(Mask, base2(Value), [""]).

apply_mask([], [], Acc)              -> unbase2(Acc);
apply_mask([$X | Ms], [V | Vs], Acc) -> apply_mask(Ms, Vs, [V | Acc]);
apply_mask([M | Ms], [_ | Vs], Acc)  -> apply_mask(Ms, Vs, [M | Acc]).

permute_mask([], [], Perms)              -> lists:map(fun unbase2/1, Perms);
permute_mask([$0 | Ms], [V | Vs], Perms) -> permute_mask(Ms, Vs, prepend(V, Perms));
permute_mask([$1 | Ms], [_ | Vs], Perms) -> permute_mask(Ms, Vs, prepend($1, Perms));
permute_mask([$X | Ms], [_ | Vs], Perms) -> permute_mask(Ms, Vs, prepend($0, Perms) ++ prepend($1, Perms)).

prepend(X, Perms) ->
    [ [X | L] || L <- Perms ].

parse_line(<<"mask = ", Mask/binary>>) ->
    {mask, binary:bin_to_list(Mask)};
parse_line(Line) ->
    {match, [[Location], [Value]]} = re:run(Line, "([0-9]+)", [global, {capture, [1], list}]),
    {memset, list_to_integer(Location), list_to_integer(Value, 10)}.

read_input(test) -> read_input("apps/fourteen/src/test.txt");
read_input(test2) -> read_input("apps/fourteen/src/test2.txt");
read_input(main) -> read_input("apps/fourteen/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    [parse_line(Line) || Line <- Lines].
