%% Copyright
-module(client).
-behaviour(gen_server).

-author("nipack").

%% API
-export([start/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define( SERVER_NAME, tcp_server ).

%% gen_server -----------------------------------------------------------------------------------------------
start( Soc, Protocol ) ->
  gen_server:start( ?MODULE, [{socket, Soc}, {protocol, Protocol}], [] ).

init( Params ) ->
  {ok, Params}.

%% handler --------------------------------------------------------------------------------------------------
%% Don't call!
handle_call( {run, _}, _From, State ) ->
  {reply, {error, []}, State}.

%% handler cast ----------------------------------------------------------------------------------------------
handle_cast( {run}, State ) ->
  client_main( State ), %% start client.
  {noreply, {ok, []}, State}.

handle_info( _Info, State ) ->
  {noreply, State}.

code_change( _OldVsn, State, _Extra ) ->
  {ok, State}.

terminate( _, State ) ->
  Soc = proplists:get_value( socket, State ),
  close_client( Soc ),
  ok.

%% private ----------------------------------------------------------------------------------------------
client_main( State ) ->
  Soc      = proplists:get_value( socket, State ),
  Protocol = proplists:get_value( protocol, State ),

  case Protocol:progress( Soc ) of
    _ ->
      close_client( Soc )
  end,
  {ok, []}.

%%-------------------------------------------------------------------------------------------------------------
%% private
%%-------------------------------------------------------------------------------------------------------------
close_client( Soc ) ->
  gen_tcp:close( Soc ).