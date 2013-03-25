%% Copyright
-module(tcp_server).
-behaviour(gen_server).

-author("nipack").

%% API
-export([start_link/1, accept_client/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link( _Args ) ->
  gen_server:start_link( {local, tcp_server}, ?MODULE, _Args, [] ).

init( _Args ) ->
  Port     = proplists:get_value( port, _Args ),
  error_logger:info_msg( "start npcached server: port ~p~n", [Port] ),
  case gen_tcp:listen( Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}] ) of
    {ok, Listen} ->
      spawn( ?MODULE, accept_client, [[{listen, Listen}] ++ _Args] ),
      {ok, {Listen, []}};
    _Other ->
      error_logger:error_msg( "failed create listen socket." ),
      {stop, "failed create listen socket."}
  end.

accept_client( Params ) ->
  Listen   = proplists:get_value( listen,   Params ),
  Protocol =
    list_to_atom( atom_to_list( proplists:get_value( protocol, Params ) ) ++ "_protocol" ),

  case gen_tcp:accept( Listen ) of
    {ok, Soc} ->
      case client:start( Soc, Protocol ) of
        {ok, Pid } ->
          gen_server:cast( Pid, {run} );
        _ ->
          error_logger:error_msg( "faild spawn client proc.~n" )
      end,
      accept_client( Params );
    _Other ->
      error
  end.

handle_call( _Req, _From, State ) ->
  {reply, {ok, []}, State}.

handle_cast( _Req, State ) ->
  {noreply, ok, State}.

handle_info( _Info, State ) ->
  {noreply, State}.

code_change( _OldVsn, State, _Extra ) ->
  {ok, State}.

terminate(_Reason, State ) ->
  Listen = proplists:get_value( listen, State ),
  gen_tcp:close( Listen ),
  ok.