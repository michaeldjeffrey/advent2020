-module(day23).
-compile(export_all).
-export([]).

-define(ONE_MILLION, 1000000).
-define(TEN_MILLION, 10000000).

turn([CurrentCup, A, B, C | Rest], UpperLimit) ->
    DestCup = get_destination(CurrentCup, Rest, UpperLimit),
    {Before, [DestCup | After]} = lists:splitwith(fun(X) -> X /= DestCup end, Rest),
    Before ++ [DestCup, A, B, C] ++ After ++ [CurrentCup].

get_destination(0, Cups, UpperLimit) -> get_destination(UpperLimit+1, Cups, UpperLimit);
get_destination(Target, Cups, UpperLimit) ->
    case lists:member(Target-1, Cups) of
        true -> Target -1;
        false -> get_destination(Target-1, Cups, UpperLimit)
    end.

n_turns(0, Cups, _) ->
    Cups;
n_turns(N, Cups, UpperLimit) ->
    case N rem 100 of
        0 ->
            io:format("Turns left: ~p~n", [N]);
        _ -> ok
    end,
    Next = turn(Cups, UpperLimit),
    n_turns(N-1, Next, UpperLimit).

rotate_to_one([1 | Rest]) -> Rest;
rotate_to_one([X | Rest]) -> rotate_to_one(Rest ++ [X]).

two_after_one(Cups) ->
    Zipped = lists:zip(Cups, lists:seq(1, ?ONE_MILLION)),
    Map = maps:from_list(Zipped),
    OneIdx = maps:get(1, Map),
    {lists:nth(OneIdx + 1 rem (?ONE_MILLION-1), Cups),
     lists:nth(OneIdx+2 rem (?ONE_MILLION-1), Cups)}.

ltoi(L) ->
    lists:flatten(lists:map(fun erlang:integer_to_list/1, L)).

itol(I) ->
    [X - $0 || X <- integer_to_list(I)].

part1(I) ->
    part1(I, 100).

part1(I, Turns) when is_number(I) ->
    part1(itol(I), Turns);
part1(I, Turns) ->
    Upper = lists:max(I),
    Cups = n_turns(Turns, I, Upper),
    ltoi(rotate_to_one(Cups)).

part2(I, Turns) when is_number(I) ->
    part2(itol(I), Turns);
part2(I, Turns) ->
    Start = I ++ lists:seq(10, ?ONE_MILLION),
    Upper = lists:max(Start),
    Cups = n_turns(Turns, Start, Upper),
    two_after_one(Cups).
