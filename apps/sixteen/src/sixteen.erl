-module(sixteen).
-import(lists, [sort/1, zip/2, seq/2, sum/1, flatten/1]).
-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(test)},
     {main, part1_solve(main)}].

part2() ->
    [{test, part2_solve(test)},
     {main, part2_solve(main)}].

part1_solve(Input) ->
    #{rules := Rules, nearby_tickets := NearbyTickets} = read_input(Input),

    AllFields = flatten(NearbyTickets),
    InvalidFields = [F || F <- AllFields, not passes_any_rules(F, Rules)],

    sum(InvalidFields).

part2_solve(Input) ->
    #{rules := Rules, nearby_tickets := NearbyTickets, my_ticket := MyTicket} = read_input(Input),

    ValidTickets = [index_fields(T) || T <- NearbyTickets, valid_ticket(T, Rules)],
    Rules1 = process_tickets(ValidTickets, Rules),
    Rules2 = sort(potential_mapping(Rules1)),

    FieldMapping = sort(narrow_to_final(Rules2)),
    multiply_departure_fields(FieldMapping, MyTicket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_ticket(Ticket, Rules) ->
    lists:all(fun(Value) -> passes_any_rules(Value, Rules) end, Ticket).

passes_any_rules(Value, Rules) ->
    lists:any(fun(Rule) -> within_range(Value, Rule) end, Rules).

process_tickets(Tickets, Rules) ->
    lists:foldl(
      fun(Ticket, Rs) -> [run_ticket(Ticket, R) || R <- Rs] end,
      Rules,
      Tickets).

index_fields(Ticket) ->
    zip(seq(1, length(Ticket)), Ticket).

potential_mapping(Rules) ->
    lists:map(
      fun(Rule) ->
              #{valid := Valid, invalid := Invalid, name := Name} = Rule,
              Potential = sets:to_list(sets:subtract(Valid, Invalid)),
              {length(Potential), Name, Potential}
      end, Rules).

narrow_to_final(Rules) ->
    {Assignments, _} =
        lists:foldl(
          %% Buckle up
          fun ({_, Name, Potential}, {Completed, Taken}) ->
                  [Final] = Potential -- Taken,
                  Completed1 = [{Final, Name} | Completed],
                  Taken1 = [Final | Taken],
                  {Completed1, Taken1}
          end,
          {[], []},
          Rules
         ),
    Assignments.

multiply_departure_fields(Fields, Values) ->
    multiply_departure_fields(Fields, Values, 1).

multiply_departure_fields([], [], Acc) -> Acc;
multiply_departure_fields([{_, <<"departure ", _/binary>>} | Fields], [Value | Values], Acc ) ->
    multiply_departure_fields(Fields, Values, Acc * Value);
multiply_departure_fields([_ | Fields], [_ | Values], Acc) ->
    multiply_departure_fields(Fields, Values, Acc).

run_ticket([], Rule) -> Rule;
run_ticket([{FieldNum, Value} | Ticket],
           #{valid := Valid, invalid := Invalid} = Rule) ->
    Rule1 =
        case within_range(Value, Rule) of
            true -> Rule#{valid := sets:add_element(FieldNum, Valid)};
            false -> Rule#{invalid := sets:add_element(FieldNum, Invalid)}
        end,
    run_ticket(Ticket, Rule1).

within_range(Value, #{values := Constraints}) ->
    %% Correct values are in 2nd or 4th positions
    case lists:sort([Value | Constraints]) of
        [_, Value, _, _, _ ] -> true;
        [_, _, _, Value, _ ] -> true;
        _ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input Handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_rule(Input) ->
    [FieldName | Values] = split(rule, Input),
    #{name => FieldName, values => to_ints(Values),
      valid => sets:new(), invalid => sets:new()}.

new_ticket(Ticket) ->
    to_ints(split(comma, Ticket)).

read_input(test) -> read_input("apps/sixteen/src/test.txt");
read_input(main) -> read_input("apps/sixteen/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    [Rules, TSection1, TSection2] = split(nlnl, Data),

    [_, MyTicket] = split(nl, TSection1),
    [_ | NearbyTickets] = split(nl, TSection2),

    #{rules => lists:map(fun new_rule/1, split(nl, Rules)),
      my_ticket => new_ticket(MyTicket),
      nearby_tickets => lists:map(fun new_ticket/1, NearbyTickets)}.

split(nlnl, Data)  -> binary:split(Data, <<"\n\n">>, [global, trim]);
split(nl, Data)    -> binary:split(Data, <<"\n">>, [global, trim]);
split(comma, Data) -> binary:split(Data, <<",">>, [global, trim]);
split(rule, Data) ->  binary:split(Data, [<<": ">>, <<" or ">>, <<"-">>], [global, trim]).

to_ints(Bins) ->
    lists:map(fun binary_to_integer/1, Bins).
