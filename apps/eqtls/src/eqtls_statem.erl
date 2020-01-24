-module(eqtls_statem).

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).

%% gen_statem state functions
-export([start/3]).

-record(state, {}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_statem:start_link({local,?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_statem callback functions
%%====================================================================

init([]) ->
    {ok, start, #state{}}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    ok.

%%====================================================================
%% gen_statem state functions
%%====================================================================

start(_, _, State) ->
    {next_state, start, State}.

%%====================================================================
%% Internal functions
%%====================================================================
