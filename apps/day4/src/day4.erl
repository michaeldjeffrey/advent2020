-module(day4).

%-export([part1/0, part2/0, read_input/1, contains_required_fields/1]).
-compile(export_all).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    Valid = lists:filter(fun (Entry) ->
                                 Keys = sets:from_list(fields(Entry, [])),
                                 RequiredKeys = sets:from_list([byr, iyr, eyr, hgt, hcl, ecl, pid]), % missing cid
                                 sets:is_subset(RequiredKeys, Keys)
                         end, Input),
    length(Valid).

part2_solve(Input) ->
    Valid = lists:filter(fun (Entry) ->
                                 Map = entry_to_map(Entry, #{}),
                                 is_valid_key(byr, fun valid_birth_year/1, Map) andalso
                                     is_valid_key(iyr, fun valid_issue_year/1, Map) andalso
                                     is_valid_key(eyr, fun valid_expiration_year/1, Map) andalso
                                     is_valid_key(hgt, fun valid_height/1, Map) andalso
                                     is_valid_key(hcl, fun valid_hair_color/1, Map) andalso
                                     is_valid_key(ecl, fun valid_eye_color/1, Map) andalso
                                     is_valid_key(pid, fun valid_passport_id/1, Map)
                         end, Input),
    length(Valid).

is_valid_key(Key, ValidFn, Map) ->
    case maps:get(Key, Map, badkey) of
        badkey -> false;
        Value -> ValidFn(Value)
    end.

fields([], Keys) -> Keys;
fields([<<"byr", _/binary>> | Rest], Keys) ->
    fields(Rest, [byr | Keys]);
fields([<<"iyr", _/binary>> | Rest], Keys) ->
    fields(Rest, [iyr | Keys]);
fields([<<"eyr", _/binary>> | Rest], Keys) ->
    fields(Rest, [eyr | Keys]);
fields([<<"hgt", _/binary>> | Rest], Keys) ->
    fields(Rest, [hgt | Keys]);
fields([<<"hcl", _/binary>> | Rest], Keys) ->
    fields(Rest, [hcl | Keys]);
fields([<<"ecl", _/binary>> | Rest], Keys) ->
    fields(Rest, [ecl | Keys]);
fields([<<"pid", _/binary>> | Rest], Keys) ->
    fields(Rest, [pid | Keys]);
fields([<<"cid", _/binary>> | Rest], Keys) ->
    fields(Rest, [cid | Keys]);
fields([_Miss | Rest], Keys) ->
    fields(Rest, [ecl | Keys]).

entry_to_map([], Map) ->
    Map;
entry_to_map([Item | Rest], Map) ->
    [Key, Value] = binary:split(Item, <<":">>),
    entry_to_map(Rest, maps:put(binary_to_atom(Key), Value, Map)).

valid_birth_year(Input) when is_number(Input)->
    in_range(1920, Input, 2002);
valid_birth_year(Input) when is_binary(Input) ->
    valid_birth_year(binary_to_integer(Input)).

valid_issue_year(Input) when is_number(Input) ->
    in_range(2010, Input, 2020);
valid_issue_year(Input) when is_binary(Input) ->
    valid_issue_year(binary_to_integer(Input)).

valid_expiration_year(Input) when is_number(Input) ->
    in_range(2020, Input, 2030);
valid_expiration_year(Input) when is_binary(Input) ->
    valid_expiration_year(binary_to_integer(Input)).

valid_height({Height, <<"cm">>}) ->
    in_range(150, Height, 193);
valid_height({Height, <<"in">>}) ->
    in_range(59, Height, 76);
valid_height(Input) when is_binary(Input) ->
    valid_height(string:to_integer(Input));
valid_height(_) -> false.

valid_hair_color(Input) ->
    case re:run(Input, "^#[a-f0-9]{6}$") of
        {match, _} -> true;
        nomatch -> false
    end.

valid_eye_color(Input) ->
    ValidColors = sets:from_list([<<"amb">>, <<"blu">>, <<"brn">>, <<"gry">>, <<"grn">>, <<"hzl">>, <<"oth">>]),
    sets:is_element(Input, ValidColors).

valid_passport_id(Input) when is_binary(Input) ->
    case re:run(Input, "^[0-9]{9}$") of
        {match, _} -> true;
        nomatch -> false
    end;
valid_passport_id(_) -> false.



in_range(Min, Value, Max) ->
    Value >= Min andalso Value =< Max.

read_input(test) -> read_input("apps/day4/src/test.txt");
read_input(main) -> read_input("apps/day4/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Entries = binary:split(Data, <<"\n\n">>, [global, trim]),
    lists:map(fun(Entry) ->
                      binary:split(Entry, [<<"\n">>, <<" ">>], [global, trim])
              end, Entries).
