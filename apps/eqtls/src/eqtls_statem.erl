-module(eqtls_statem).

-behaviour(gen_statem).

%% API
-export([get_flight/1,
	 start_link/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).

%% gen_statem state functions
-export([negotiated/3,
	 start/3,
	 wait_sh/3]).

-record(state, {mode,
		history = []}).


-include_lib("ssl/src/ssl_handshake.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/tls_handshake_1_3.hrl").
-include_lib("public_key/include/public_key.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link(Mode) ->
    gen_statem:start_link(?MODULE, [Mode], []).

get_flight(Pid) ->
    call(Pid, {get_flight}).

%%====================================================================
%% gen_statem callback functions
%%====================================================================

init([Mode]) ->
    {ok, start, #state{mode = Mode}}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    ok.

%%====================================================================
%% gen_statem state functions
%%====================================================================

start({call, From}, {get_flight}, #state{mode = client} = StateData) ->
    ClientHello = client_hello(StateData),
    {next_state, wait_sh, StateData, [{reply, From, ClientHello}]};
start(cast, {put_flight, [Raw]}, #state{mode = server,
					history = History} = StateData0) ->
    Version = {3,4},
    <<?BYTE(Type), ?UINT24(_), Body/binary>> = Raw,
    _ClientHello = tls_handshake:decode_handshake(Version, Type, Body),
    %% TODO: process ClientHello
    {next_state, negotiated, StateData0#state{history = [Raw|History]}};
start(_, _, StateData) ->
    {next_state, start, StateData}.

wait_sh({call, From}, _, StateData) ->
    {next_state, start, StateData, [{reply, From, restarted}]}.

negotiated({call, From}, {get_flight}, #state{mode = server} = StateData) ->
    {next_state, wait_cert, StateData, [{reply, From, <<"SH EE Cert CV F">>}]}.

%%====================================================================
%% Internal functions
%%====================================================================

call(FsmPid, Event) ->
    try gen_statem:call(FsmPid, Event)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{{shutdown, _},_} ->
	    {error, closed}
    end.

client_hello(_StateData) ->
    Version = {3,4},
    LegacyVersion = {3,3},
    AvailableCipherSuites = ssl_cipher:filter_suites(ssl_cipher:suites(Version)),
    Groups = ssl:groups(),
    KeyShare = ssl_cipher:generate_client_shares(Groups),
    #key_share_client_hello{client_shares = ClientShares0} = KeyShare,
    %% Keep only public keys
    ClientShares = lists:map(fun kse_remove_private_key/1, ClientShares0),


    SessionId = crypto:strong_rand_bytes(32),
    SignatureSchemes = tls_v1:default_signature_algs(Version),
    %% TODO: ticket data, handshake history, alpn
    Extensions = #{sni => #sni{hostname = "localhost"},
		   ec_point_formats =>
		       #ec_point_formats{ec_point_format_list = [?ECPOINT_UNCOMPRESSED]},
		   elliptic_curves =>
		       #supported_groups{supported_groups = Groups},
		   signature_algs =>
		       #signature_algorithms{signature_scheme_list = SignatureSchemes},
		   client_hello_versions =>
		       #client_hello_versions{versions = [Version]},
		   signature_algs_cert =>
		       #signature_algorithms_cert{signature_scheme_list = SignatureSchemes},
		   key_share =>
		       #key_share_client_hello{client_shares = ClientShares}
		  },

    ClientHello0 = #client_hello{session_id = SessionId,
				client_version = LegacyVersion,
				cipher_suites = AvailableCipherSuites,
				compression_methods = [0],
				random = crypto:strong_rand_bytes(32),
				extensions = Extensions
			       },
    ClientHello = tls_handshake:encode_handshake(ClientHello0, Version),
    [iolist_to_binary(ClientHello)].


kse_remove_private_key(#key_share_entry{
                      group = Group,
                      key_exchange =
                          #'ECPrivateKey'{publicKey = PublicKey}}) ->
    #key_share_entry{
       group = Group,
       key_exchange = PublicKey};
kse_remove_private_key(#key_share_entry{
                      group = Group,
                      key_exchange =
                          {PublicKey, _}}) ->
    #key_share_entry{
       group = Group,
       key_exchange = PublicKey}.
