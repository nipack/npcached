%% Copyright
-module(kvs).
-behaviour(supervisor).

-author("nipack").

%% API
-export([init/1]).
-export([start_link/1, get/1, set/4, delete/1, incr/2, decr/2]).
-export([sweep/0]).

start_link( _Args ) ->
  supervisor:start_link( {local, kvs}, ?MODULE, _Args ).

init( _Args ) ->
  Port     = proplists:get_value( port,     _Args ),
  Protocol = proplists:get_value( protocol, _Args ),
  Nodes    = proplists:get_value( nodes,    _Args ),

  {ok, {{one_for_all, 3, 10},
    [{database,    {database, start_link, [Nodes]}, permanent, 1000, worker, [database]},
     {sweeper,     {sweeper, start, [database]},    permanent, 1000, worker, [sweeper]},
     {tcp_server,  {tcp_server, start_link,
       [[{port, Port}, {protocol, Protocol}]]},     permanent, 5000, worker, [tcp_server]}]}}.

get( Key ) ->
  gen_server:call( database, {get, Key} ).

set(Key, Value, Flags, Expire) ->
  gen_server:call( database, {set, Key, Value, Flags, Expire} ).

delete( Key ) ->
  gen_server:call( database, {delete, Key} ).

incr( Key, Dist ) ->
  gen_server:call( database, {incr, Key, Dist} ).

decr( Key, Dist ) ->
  gen_server:call( database, {decr, Key, Dist} ).

sweep() ->
  gen_server:call( database, {sweep} ).
