-module(day19try2).
-compile(export_all).

-record(state, {rule, input, stack=[], backtrack=[], getter}).

part1() ->
    %% Guessed 54, not correct (Incorrect parsing of rule "115: 86 | 52")
    {Rules, Messages} = input(main),
    RuleGetter = fun(Get) -> maps:get(Get, Rules) end,
    lists:sum([match(RuleGetter, Message) || Message <- Messages]).

part2() ->
    %% Guessed 282, not correct (Rejection matching [_Char2 | _Rest] wasn't letting exhausted strings through)
    {Rules, Messages} = input(main),
    {_, Eight} = parse_rule(<<"8: 42 | 42 8">>),
    {_, Eleven} = parse_rule(<<"11: 42 31 | 42 11 31">>),

    RuleGetter = fun(8)   -> Eight;
                    (11)  -> Eleven;
                    (Get) -> maps:get(Get, Rules)
                 end,

    lists:sum([match(RuleGetter, Message) || Message <- Messages]).

match(Rules, String) ->
    State = #state{rule=Rules(0), getter=Rules, input=String},
    match(State).

match(#state{rule={char, Char}, input=[Char], stack=[]})            -> done(success);
match(#state{rule={char, Char}, input=[Char|_], stack=[_|_]}=State) -> char_hit(State);
match(#state{rule={char, _Char}, backtrack=[]})                     -> done(failure);
match(#state{rule={char, _Char}}=State)                             -> char_miss(State);
match(#state{rule={group, [Rule | Next]}}=State)                    -> group(Rule, Next, State);
match(#state{rule={split, Left, Right}}=State)                      -> split(Left, Right, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Match functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

done(success) -> 1;
done(failure) -> 0.

char_hit(State) ->
    State1 = eat_char(State),
    State2 = enter_rule_from_stack(State1),
    match(State2).

char_miss(State) ->
    State1 = reject_char(State),
    match(State1).

group(Rule, NextRules, State) ->
    State1 = queue_rules(NextRules, State),
    State2 = enter_rule(Rule, State1),
    match(State2).

split(One, Two, State) ->
    State1 = store_backtrack(Two, State),
    match(State1#state{rule=One}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% State functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enter_rule_from_stack(#state{stack=[Rule | Rest]}=State) ->
    enter_rule(Rule, State#state{stack=Rest}).

eat_char(#state{input=[_|Input]}=State) ->
    State#state{input=Input}.

reject_char(#state{backtrack=[{Rule, String, Stack} | Backtracks]}=State) ->
    State#state{rule=Rule, input=String, stack=Stack, backtrack=Backtracks}.

enter_rule(Rule, #state{getter=GetFn}=State) ->
    State#state{rule=GetFn(Rule)}.

queue_rules(ToQueue, #state{stack=Stack}=State) ->
    State#state{stack=ToQueue ++ Stack}.

store_backtrack(Rule, #state{input=Input, stack=Stack, backtrack=BT}=State) ->
    State#state{backtrack=[{Rule, Input, Stack} | BT]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_rule(Bin) ->
    [Key, Value] = split(Bin, colon),
    {b2i(Key), parse_value(Value)}.

parse_value(<<$", Char, $">>) ->
    {char, Char};
parse_value(Bin) ->
    case split(Bin, pipe) of
        [One, Two] -> {split, parse_group(One), parse_group(Two)};
        [One] -> parse_group(One)
    end.

parse_group(Bin) ->
    Nums = split(Bin, space),
    {group, [b2i(Num) || Num <- Nums]}.

b2i(B) ->
    binary_to_integer(B).

input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    [Rules, Input] = split(Data, nlnl),
    RLines = split(Rules, nl),

    {maps:from_list(lists:map(fun parse_rule/1, RLines)),
     lists:map(fun binary_to_list/1, split(Input, nl))}.

split(Bin, nlnl) -> binary:split(Bin, <<"\n\n">>, [global, trim]);
split(Bin, nl) -> binary:split(Bin, <<"\n">>, [global, trim]);
split(Bin, pipe) -> binary:split(Bin, <<" | ">>, [global, trim]);
split(Bin, space) -> binary:split(Bin, <<" ">>, [global, trim]);
split(Bin, colon) -> binary:split(Bin, <<": ">>, [global, trim]).
