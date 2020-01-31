-module(eqtls).

-export([client/0,
	 server/0,
	 get_flight/1,
	 put_flight/2,
	 set_transport_parameters/2]).

%%====================================================================
%% API functions
%%====================================================================

client() ->
    eqtls_sup:start_child([client]).

server() ->
    eqtls_sup:start_child([server]).

%% Including transport parameters in the TLS handshake provides
%% integrity protection for these values.

%%    enum {
%%       quic_transport_parameters(0xffa5), (65535)
%%    } ExtensionType;

%% The "extension_data" field of the quic_transport_parameters extension
%% contains a value that is defined by the version of QUIC that is in
%% use.
set_transport_parameters(_Pid, _Params) ->
    ok.

get_flight(Pid) ->
    eqtls_statem:get_flight(Pid).

put_flight(_Pid, _Handshake) ->
    ok.
