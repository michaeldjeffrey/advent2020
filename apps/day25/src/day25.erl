-module(day25).
-compile(export_all).
-export([]).

-define(SUBJECT, 7).
-define(DIVISOR, 20201227).

find_key(Card, Door) ->
    find_key(Card, Door, 1, 1).

find_key(Card, _, Card, Key) -> Key;
find_key(Card, Door, Value, Key) ->
    find_key(
      Card, Door,
      (Value * ?SUBJECT) rem ?DIVISOR,
      (Key * Door) rem ?DIVISOR).

part1(IT) ->
    [Card, Door] = input(IT),
    find_key(Card, Door).

part2() ->
    'still 6 stars short :('.

input(test) ->
    [5764801, 17807724];
input(main) ->
    [2084668, 3704642].
