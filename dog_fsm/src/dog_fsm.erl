-module(dog_fsm).

-behavior(gen_statem).

%% Public API
-export([start/0,stop/0,pet/0,give_squirrel/0]).

%% Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).

%% State callbacks
-export([bark/3,wag_tail/3,sit/3]).


name() -> ?MODULE.

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
    gen_statem:start({local,name()}, ?MODULE, [], []).
pet() ->
    gen_statem:call(name(), pet, 2000).
give_squirrel() ->
    gen_statem:call(name(), squirrel, 2000).
stop() ->
    gen_statem:stop(name()).


%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
init([]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = bark, Data = 0,
    {
      %% Start in bark state
      ok,State,Data, 
      %% With timeout events enabled from the start
      [{state_timeout, 2000, bark_again}] 
    }.
callback_mode() -> state_functions.

%%% state callback(s)

bark(state_timeout, bark_again, Data) ->
    io:format("Dog says: BARK! BARK!~n"),
    {
      next_state,bark,Data,
      [{state_timeout, 2000, bark_again}]
    };
bark({call,From}, pet, Data) ->
    io:format("Dog says: BARK! BARK!~n"),
    %% Go to 'wag_tail' and reply
    %% that the resulting status is 'wag_tail'
    %% Or if enough time passes without events, keep wagging :) 
    {
      next_state,wag_tail,Data,
      [{reply,From,wag_tail}, {state_timeout, 2000, wag_again}]
    };
bark(EventType, EventContent, Data) ->
    io:format("Dog is confused~n"),
    %% TODO send no_reply or something IDK -- would that make it nicer for clients?
    handle_event(EventType, EventContent, Data).



wag_tail(state_timeout, wag_again, Data) ->
    io:format("Dog wags its tail~n"),
    {
      next_state,wag_tail,Data,
      [{state_timeout, 2000, wag_again}]
    };
wag_tail({call,From}, pet, Data) ->
    io:format("Dog wags its tail~n"),
    %% Go to 'sit' and reply
    %% that the resulting status is 'sit'
    {
      next_state,sit,Data,
      [{reply,From,sit},{state_timeout,2000,sit_again}]
    };
wag_tail(EventType, EventContent, Data) ->
    io:format("Dog is confused~n"),
    handle_event(EventType, EventContent, Data).



sit(state_timeout, sit_again, Data) ->
    io:format("Dog is sitting. Gooooood boy!~n"),
    {
      next_state,sit,Data,
      [{state_timeout, 2000, sit_again}]
    };
sit({call,From}, squirrel, Data) ->
    io:format("Dog is sitting. Gooooood boy!~n"),
    %% Go to 'bark' and reply
    %% that the resulting status is 'bark'
    {
      next_state,bark,Data,
      [{reply,From,bark},{state_timeout,2000,bark_again}]
    };
sit(EventType, EventContent, Data) ->
    io:format("Dog is confused~n"),
    handle_event(EventType, EventContent, Data).


%% TODO mandatory callback needs rethinking --its copied from on/off button
%% TODO ?? Why can't I get the current state? Maybe sys module already handles that?
%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.




%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%

