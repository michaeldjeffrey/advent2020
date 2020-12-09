-module(day8).

-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Instructions) ->
    {ok, Pid} = vm:start_link(Instructions),
    ok = run_loop(Pid),
    Accumulator = vm:accumulator(Pid),
    vm:delete(Pid),
    Accumulator.

part2_solve(Instructions) ->
    Size = array:size(Instructions),
    Vms = lists:map(fix_vm_with_instructions(Instructions), lists:seq(0, Size-1)),
    lists:foreach(fun run_loop/1, Vms),
    lists:foreach(fun vm:delete/1, Vms).


fix_vm_with_instructions(Instructions) ->
    fun (IndexToFix) ->
            {ok, Pid} = vm:start_link(Instructions),
            vm:fix_instruction(Pid, IndexToFix),
            Pid
    end.


run_loop(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            run_loop(Pid, vm:tick(Pid));
        false ->
            ok
    end.

run_loop(Pid, ok) -> run_loop(Pid, vm:tick(Pid));
run_loop(_Pid, loop) -> ok;
run_loop(Pid, finished) ->
    io:format("Pid ~p finished execution ~p~n", [Pid, vm:report(Pid)]),
    ok.

read_input(test) -> read_input("apps/day8/src/test.txt");
read_input(main) -> read_input("apps/day8/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    List = binary:split(Data, <<"\n">>, [global, trim]),
    array:from_list(lists:map(fun(Bin) ->
                                     [Op, Amount] = binary:split(Bin, <<" ">>),
                                     {binary_to_atom(Op), binary_to_integer(Amount)}
                             end, List)).
