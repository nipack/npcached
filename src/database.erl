%% Copyright
-module(database).
-behaviour(gen_server).

-include_lib( "stdlib/include/qlc.hrl" ).
-include("values.hrl").

-author("nipack").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([setup/1, link_nodes/1]).

%% for test
-export([get_value/1, set_value/4, set_value_simple/2]).
-export([delete_value/1, incr_decr_value/3]).
-export([do_sweep/0]).

%% gen_server -----------------------------------------------------------------------------------------------
start_link( _Args ) ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, _Args, [] ).

init( _Args ) ->
  R = case mnesia:start() of
    {error, _} ->
      error_logger:error_msg( "failed start mnesia on database.~n" ),
      {stop, "failed start mnesia."};

    _ ->
      error_logger:info_msg( "start database. replicate ~p~n", [_Args] ),
      link_nodes( _Args ),
      {ok, []}
  end,
  R.

%% handler --------------------------------------------------------------------------------------------------
handle_call( {set, Key, Value, Flags, Expire}, _From, State ) ->
  R = ?MODULE:set_value( Key, Value, Flags, Expire ),
  {reply, R, State};
handle_call( {get, Key}, _From, State ) ->
  R = ?MODULE:get_value( Key ),
  {reply, R, State };
handle_call( {delete, Key}, _From, State ) ->
  R = ?MODULE:delete_value( Key ),
  {reply, R, State};
handle_call( {incr, Key, Dist}, _From, State ) ->
  R = ?MODULE:incr_decr_value( Key, Dist, true ),
  {reply, R, State};
handle_call( {decr, Key, Dist}, _From, State ) ->
  R = ?MODULE:incr_decr_value( Key, Dist, false ),
  {reply, R, State};

handle_call( {sweep}, _From, State ) ->
  R = ?MODULE:do_sweep(),
  {reply, R, State}.

%% handler cast ----------------------------------------------------------------------------------------------
handle_cast( {set, Key, Value, Flags, Expire}, State ) ->
  R = ?MODULE:set_value( Key, Value, Flags, Expire ),
  {noreply, R, State};
handle_cast( {sweep}, State ) ->
  R = ?MODULE:do_sweep(),
  {noreply, R, State}.

handle_info( _Info, State ) ->
  {noreply, State}.

code_change( _OldVsn, State, _Extra ) ->
  {ok, State}.

terminate( _, _State ) ->
  mnesia:stop(),
  ok.

%% implements ------------------------------------------------------------------------------------------------

setup( [] ) ->
  setup( ram_copies );
setup( Stg ) ->
  mnesia:create_schema([node()]),
  mnesia:change_table_copy_type( schema, node(), Stg ),
  mnesia:start(),
  case mnesia:create_table( values,
    [{attributes, record_info(fields, values)}]) of
    {atomic, ok} ->
      mnesia:change_table_copy_type( values, node(), Stg ),
      ok;
    _ ->
      error
  end,
  mnesia:stop().

link_nodes( [] ) ->
  {ok, []};
link_nodes( Nodes ) ->
  try
    %% ping.
    lists:foreach( fun( Node ) -> net_adm:ping( Node ) end, Nodes ),

    mnesia:change_config( extra_db_nodes, Nodes ),
    mnesia:add_table_copy( values, node(), ram_copies )
  catch
    exit:_Reason ->
      error_logger:error_msg( "~s~n", [_Reason] )
  end,
  {ok,[]}.

%% -------------------------------------------------------------------------------------------------------------
set_value( Key, Value, Flags, Expire ) ->
  try
    FlagsInt  = get_integer( Flags ),
    ExpireInt = get_integer( Expire ),
    if
      Expire =:= 0 ->
        Timestamp = 0;
      true ->
        Timestamp = get_timestamp()
    end,
    set_value_impl( [Key, Value, FlagsInt, ExpireInt + Timestamp, Timestamp, Timestamp] )
  catch
    error:_Reason ->
      {error, []}
  end.

set_value_simple( Key, Value ) ->
  case get_value_impl( Key ) of
    {ok, { _, _, _, Flags, Expire, Created, _}} ->
      Timestamp = get_timestamp(),
      set_value_impl( [Key, Value, Flags, Expire, Created, Timestamp] ),
      R = {ok, Value};
    _Val ->
      set_value_impl( [Key, Value, 0, 0] ),
      R = {ok, []}
  end,
  R.

get_value( Key ) ->
  case get_value_impl( Key ) of
    {ok, { _, _, Value, _, _, _, _}} ->
      R = {ok, Value};
    Other ->
      R = Other
  end,
  R.

delete_value( Key ) ->
  Fun = fun() -> mnesia:delete( {values, Key} ) end,
  case mnesia:transaction( Fun ) of
    {atomic, _} ->
      {ok, []};
    _ ->
      {notfound, []}
  end.

incr_decr_value( Key, Dist, Vct ) ->
  try
    case get_value( Key ) of
      {ok, Value} ->
        ValInt  = get_integer( Value ),
        DistInt = get_integer( Dist ),

        case Vct of
          true ->
            R = set_value_simple( Key, integer_to_list( ValInt + DistInt ) );
          false ->
            R = set_value_simple( Key, integer_to_list( ValInt - DistInt ) )
        end;
      _ ->
        R = {notfound, []}
    end,
    R
  catch
    error: _Reason ->
      {notfound, []}
  end.

do_sweep() ->
  Timestamp = get_timestamp(),
  R    = qlc:q( [X#values.key || X <- mnesia:table( values ), X#values.expire < Timestamp, X#values.expire =/= 0 ] ),
  Fun  = fun() ->
    Keys = qlc:e( R ),
    Del  = fun( Key ) -> mnesia:delete( {values, Key} ) end,

    lists:foreach( Del, Keys ),
    length( Keys )
  end,

  case mnesia:transaction( Fun ) of
    {atomic, Count} ->
      {ok, Count};
    _ ->
      {error, []}
  end.

get_timestamp() ->
  {Mega, Sec, _} = now(),
  Mega * 1000000 + Sec.

%%----------------------------------------------------------------------------------------------------------------------
%% Converter.
%%----------------------------------------------------------------------------------------------------------------------

get_integer( Val ) ->
  case is_integer( Val ) of
    false ->
      R = list_to_integer( Val );
    _ ->
      R = Val
  end,
  R.

%%----------------------------------------------------------------------------------------------------------------------
%% private functions.
%%----------------------------------------------------------------------------------------------------------------------

get_value_impl( Key ) ->
  Timestamp = get_timestamp(),
  Fun   = fun() -> mnesia:read( values, Key, read )end,
  case mnesia:activity( transaction, Fun ) of
    [] ->
      R = {notfound, []};
    Result ->
      Tmp = hd( Result ),
      { _, _, _, _, Expire, _, _} = Tmp,
      if
        Expire =:= 0 ->
          R = {ok, Tmp};
        Expire =< Timestamp ->
          R = {notfound, []};
        true ->
          R = {ok, Tmp}
      end
  end,
  R.

set_value_impl( [Key, Value, Flags, Expire] ) ->
  Timestamp = get_timestamp(),
  set_value_impl( [Key, Value, Flags, Expire, Timestamp, Timestamp] );
set_value_impl( [Key, Value, Flags, Expire, Created, Updated] ) ->
  V = #values {
      key         = Key,
      value       = Value,
      flags       = Flags,
      expire      = Expire,
      created_at  = Created,
      updated_at  = Updated },
  Fun = fun() -> mnesia:write( V ) end,
  mnesia:transaction( Fun ),
  {ok, []};
set_value_impl( _ ) ->
  {error, []}.
