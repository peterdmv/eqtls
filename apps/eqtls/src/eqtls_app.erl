%%%-------------------------------------------------------------------
%% @doc eqtls public API
%% @end
%%%-------------------------------------------------------------------

-module(eqtls_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eqtls_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
