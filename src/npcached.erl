%% Copyright
-module(npcached).
-behaviour(application).

-author("nipack").

%% API
-export([main/0]).
-export([start/2, stop/1]).
-export([setup/1, link_nodes/1]).

%% test start point.
main() ->
  start( normal, []).

start( normal, _Args ) ->
  case validate_options() of
    {ok, _} ->
      {ok, Port}     = application:get_env( npcached, port ),
      {ok, Protocol} = application:get_env( npcached, commnication_protocol ),
      {ok, Nodes}    = application:get_env( npcached, replicate_nodes ),

      kvs:start_link(
        [{port, Port}, {protocol, Protocol}, {nodes, Nodes}]
      );
    _ ->
      stop
  end.

stop( _State ) ->
  ok.

setup( Options ) ->
  database:setup( Options ).

link_nodes( Nodes ) ->
  database:link_nodes( Nodes ).

%% private -------------------------------------------------------------
validate_options() ->
  try
    {ok, Port} = application:get_env( npcached, port ),
    _ = integer_to_list( Port ),
    {ok, _CommIf} = application:get_env( npcached, commnication_protocol ),
    {ok, _Nodes}  = application:get_env( npcached, replicate_nodes ),
    {ok, []}
  catch
    error:_Reason ->
      error_logger:error_msg( "~p~n", [_Reason] ),
      {error, []};
    exit:_Reason ->
      error_logger:error_msg( "~p~n", [_Reason] ),
      {error, []}
  end.


