%%%-------------------------------------------------------------------
%% @doc eqtls top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eqtls_sup).

-behaviour(supervisor).

%% API
-export([start_child/1,
	 start_link/1]).

%% Supervisor callback
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    {ok, {SupFlags, child_specs()}}.

%%====================================================================
%% Internal functions
%%====================================================================

child_specs() ->
    [#{id => eqtls_statem,
      start => {eqtls_statem, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [eqtls_statem]}].
