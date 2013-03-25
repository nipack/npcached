%% Copyright
-module(sweeper).

-author("nipack").

%% API
-export([start/1]).
-export([progress/1]).

%%define
-define( SWEEP_INTERVAL, 60000 ).

start( _Args ) ->
  error_logger:info_msg( "start sweeper~n" ),
  Pid = spawn( ?MODULE, progress, [_Args] ),
  {ok, Pid}.

progress( Mod ) ->
  receive
  after ?SWEEP_INTERVAL ->
    _R = kvs:sweep(),
    progress( Mod )
  end.

