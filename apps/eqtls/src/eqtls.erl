-module(eqtls).

-export([client/0]).

%%====================================================================
%% API functions
%%====================================================================

client() ->
    eqtls_sup:start_child([]).
