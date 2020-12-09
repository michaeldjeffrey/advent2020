-module(vm).

-behaviour(gen_server).

%% API
-export([start_link/1,
         fix_instruction/2,
         tick/1,
         accumulator/1,
         delete/1,
         report/1]).

%% Genserver
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
                instructions = array:new() :: array:array(),
                cursor = 0 :: non_neg_integer(),
                accumulator = 0 :: integer(),
                history = sets:new() :: sets:set()
               }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Instructions) ->
    gen_server:start_link(?MODULE, Instructions, []).

fix_instruction(Pid, Index) ->
    gen_server:cast(Pid, {fix, Index}).

tick(Pid) ->
    gen_server:call(Pid, tick).

accumulator(Pid) ->
    gen_server:call(Pid, accumulator).

delete(Pid) ->
    gen_server:cast(Pid, delete).

report(Pid) ->
    gen_server:call(Pid, report).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callbacks                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Instructions) ->
    {ok, #state{instructions=Instructions}}.

handle_call(report, _From, State) ->
    Report = lists:flatten(io_lib:format("[accumulator:~p]", [State#state.accumulator])),
    {reply, Report, State};
handle_call(tick, _From, State) ->
    {Res, State1} =
        case get_instruction(State) of
            finished -> {finished, State};
            loop -> {loop, State};
            Inst -> {ok, run_instruction(Inst, State)}
        end,
    {reply, Res, State1};
handle_call(accumulator, _From, State) ->
    {reply, State#state.accumulator, State};
handle_call(Request, From, State) ->
    io:format("Unhandled call~p: ~p~nState: ~p~n", [From, Request, State]),
    {reply, ok, State}.


handle_cast({fix, Index}, State) ->
    #state{instructions = Instructions0} = State,
    case fix(Index, Instructions0) of
        nothing_to_fix ->
            {stop, normal, State};
        {fixed, NewInstruction} ->
            Instructions1 = array:set(Index, NewInstruction, Instructions0),
            {noreply, State#state{instructions=Instructions1}}
    end;
handle_cast(delete, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    io:format("Unhandled cast: ~p~nState: ~p~n", [Request, State]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_instruction(Instruction, #state{cursor=Cursor, accumulator=Accum, history=History} = State) ->
    NewState =
        case Instruction of
            {nop, _} -> State#state{cursor=Cursor+1};
            {acc, Amount} -> State#state{cursor=Cursor+1, accumulator=Accum+Amount};
            {jmp, Amount} -> State#state{cursor=Cursor+Amount}
        end,
    NewState#state{history=sets:add_element(Cursor, History)}.

fix(Index, Instructions) ->
    case array:get(Index, Instructions) of
        {acc, _} ->
            nothing_to_fix;
        {jmp, Amount} ->
            {fixed, {nop, Amount}};
        {nop, Amount} ->
            {fixed, {jmp, Amount}}
    end.

get_instruction(#state{cursor=Cursor, instructions=Instructions, history=History}) ->
    case {sets:is_element(Cursor, History), array:get(Cursor, Instructions)} of
        {true, _} -> loop;
        {false, undefined} -> finished;
        {false, Instruction} -> Instruction
    end.
