-module(day23try2).

-export([part1/0, part2/0]).

part1() ->
    I = itol(247819356),
    Input = #{current_cup => 2, cups => circle_from(I), round => 1},
    Output = tick_n(Input, 100),
    [_ | Answer] = rotate_to_one(Output),
    ltoi(Answer).


part2() ->
    %% Guessed : 367512943116
    %% Too high
    %% Guessed : 12621748849
    %% Correct
    I = itol(247819356) ++ lists:seq(10, 1000000),
    Input = #{current_cup => hd(I), cups => circle_from(I), round => 1},
    #{cups := Output} = tick_n(Input, 10000000),
    #{ 1 := One} = Output,
    #{ One := Two } = Output,
    {One, Two, One * Two}.

tick_n(Input, 0) -> Input;
tick_n(Input, N) -> tick_n(tick(Input), N-1).

tick(#{current_cup := CurrentCup, cups := Cups, round := Round}) ->
    print_round(Round),

    {Removed, NextCurrent, Cups1} = remove_three(CurrentCup, Cups),
    TargetCup                     = get_dest_cup(CurrentCup-1, Cups1),
    Cups2                         = insert_after(Removed, TargetCup, Cups1),

    #{current_cup => NextCurrent,
      cups => Cups2,
      round => Round + 1
     }.

print_round(Round) ->
    if
        Round rem 50000 == 0 -> io:format("Round: ~p/10,000,000 \r", [Round]);
        true -> ok
    end,

circle_from([Head|Tail]) ->
    circle_from([Head|Tail]++[Head, nothing], #{}).

circle_from([_, nothing], Map) -> Map;
circle_from([A, B | List], Map) ->
    circle_from([B | List], Map#{A => B}).

remove_three(CurrentCup, Cups) ->
    One   = maps:get(CurrentCup, Cups),
    Two   = maps:get(One, Cups),
    Three = maps:get(Two, Cups),
    Next  = maps:get(Three, Cups),

    Removed = [One, Two, Three],
    CupsGap = maps:without(Removed, Cups),
    CupsClosed = CupsGap#{CurrentCup := Next},

    {Removed, Next, CupsClosed}.

insert_after([One, Two, Three], Target, Cups) ->
    #{Target := AfterTarget} = Cups,
    Cups#{Target => One,
      One => Two,
      Two => Three,
      Three => AfterTarget}.

get_dest_cup(0, Cups) ->
    get_dest_cup(1000000, Cups);
get_dest_cup(Current, Cups) ->
    case maps:is_key(Current, Cups) of
        true -> Current;
        false -> get_dest_cup(Current-1, Cups)
    end.

rotate_to_one(#{cups := Cups}) -> rotate_to_one(Cups);
rotate_to_one(Cups)            -> rotate_to_one(Cups, 1, []).

rotate_to_one(Map, _Last, Acc) when Map =:= #{} ->
    lists:reverse(Acc);
rotate_to_one(Map, Key, Acc) ->
    {Val, Map2} = maps:take(Key, Map),
    rotate_to_one(Map2, Val, [Key|Acc]).

ltoi(L) ->
    lists:flatten(lists:map(fun erlang:integer_to_list/1, L)).

itol(I) ->
    [X - $0 || X <- integer_to_list(I)].
