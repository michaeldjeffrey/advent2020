-module(nineteen).
-compile(export_all).
-export([]).

next_valid(Incoming) ->
    io:format("Next valid ~p~n", [Incoming]).

one(Rule, Map) ->
    case Map of
        #{Rule := {char, Value}} ->
            Value;
        #{Rule := [One, Two]} when is_list(One) ->
            Left = lists:flatten(lists:map(fun(R) -> one(R, Map) end, One)),
            Right = lists:flatten(lists:map(fun(R) -> one(R, Map) end, Two)),
            "("++Left++"|"++Right++")";
        #{Rule := [One, Two]} when is_number(One) ->
            Left = one(One, Map),
            Right = one(Two, Map),
            lists:flatten([$(, Left, $|, Right, $)]);
        #{Rule := List} when is_list(List) ->
            lists:flatten(lists:map(fun(R) -> one(R, Map) end, List))
    end.


with(R) ->
    lists:flatten(io_lib:format("~s", [one(R, test())])).

test() ->
    #{0 => [4, 1, 5],
      1 => [[2, 3], [3, 2]],
      2 => [[4, 4], [5, 5]],
      3 => [[4, 5], [5, 4]],
      4 => {char, "a"},
      5 => {char, "b"}
     }.

go({Rules, Input}) ->
    Reg = lists:flatten(io_lib:format("^~s$", [one(0, Rules)])),
    lists:sum(lists:map(fun(I) -> case re:run(I, Reg) of
                                      {match, _} -> 1;
                                      _ -> 0
                                  end
                        end, Input)).

parse_rule(L) when is_binary(L) ->
    parse_rule(binary:split(L, [<<": ">>, <<" | ">>, <<" ">>], [global, trim]));
parse_rule([Key, L1, L2, R1, R2]) ->
    {b2i(Key), [[b2i(L1), b2i(L2)], [b2i(R1),  b2i(R2)]]};
parse_rule([Key, <<$", Char, $">>]) ->
    {b2i(Key), {char, Char}};
parse_rule([Key | Nums]) ->
    {b2i(Key), lists:map(fun b2i/1, Nums)}.


b2i(B) ->
    binary_to_integer(B).


input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    [Rules, Input] = binary:split(Data, <<"\n\n">>, [global]),
    RLines = binary:split(Rules, <<"\n">>, [global]),

    {maps:from_list(lists:map(fun parse_rule/1, RLines)),
     lists:map(fun binary_to_list/1, binary:split(Input, <<"\n">>, [global, trim]))}.
