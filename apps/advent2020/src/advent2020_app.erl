%%%-------------------------------------------------------------------
%% @doc advent2020 public API
%% @end
%%%-------------------------------------------------------------------

-module(advent2020_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    advent2020_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
