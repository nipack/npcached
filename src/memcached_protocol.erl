%% Copyright
-module(memcached_protocol).

-author("nipack").

%% API
-export([progress/1]).

progress( Soc ) ->
  try
    case gen_tcp:recv( Soc, 0 ) of
      {ok, Line} ->
        Token = string:tokens( binary_to_list( Line ), " \r\n" ),
        case exec_command( Token, Soc ) of
          {close, _} ->
            {closed, []};
          _ ->
            progress( Soc )
        end;

      _Other ->
        {closed, []}
    end
  catch
    error:_ ->
      send_error( Soc );
    exit:_ ->
      {closed,{}}
  end.

%%------------------------------------------------------------------------------------------------
%% parse and execute command.
%%------------------------------------------------------------------------------------------------
exec_command( Token, Soc ) ->
  if
    length( Token ) > 0 ->
      {H, T} = lists:split( 1, Token ),
      R = string:to_lower( hd( H ) ),
      exec_command_impl( [R] ++ T, Soc );
    true ->
      send_error( Soc )
  end.

exec_command_impl( ["get", Key], Soc ) ->
  case kvs:get( Key ) of
    {ok, Value} ->
      gen_tcp:send( Soc,
        io_lib:format( "VALUE ~s 0 ~w\r\n~s\r\nEND\r\n", [Key, length(Value), Value] ) );
    _ ->
      send_error( Soc )
  end,
  {ok, []};
 exec_command_impl( ["set", Key, Flags, Expire, Len], Soc ) ->
  try
    %% Convert.
    ExpireInt = list_to_integer( Expire ),
    LenInt    = list_to_integer( Len ),

    if
      ExpireInt < 0 ; LenInt < 0 ->
        send_error( Soc );
      true ->
        Callback = fun( Value ) ->
          case kvs:set( Key, Value, Flags, ExpireInt ) of
            {ok, _} ->
              gen_tcp:send( Soc, "STORED\r\n" );
            _Other ->
              send_error( Soc )
          end
        end,
        read_value( Soc, LenInt, [], Callback )
    end
  catch
    error:_Reason ->
      send_error( Soc ),
      {ok, []}
  end;
exec_command_impl( ["delete", Key], Soc ) ->
  case kvs:delete( Key ) of
    {ok, _} ->
      gen_tcp:send( Soc, "DELETED\r\n" ),
      {ok, []};
    _Other ->
      gen_tcp:send( Soc, "NOT FOUND\r\n" ),
      {ok, []}
  end;
exec_command_impl( ["incr", Key, Dist], Soc ) ->
  try
    DistInt = list_to_integer( Dist ),
    Fun = fun() -> kvs:incr( Key, DistInt ) end,
    do_incr_decr_impl( Soc, Fun )
  catch
    error:_Reason ->
      send_error( Soc ),
      {ok, []}
  end;
exec_command_impl( ["decr", Key, Dist], Soc ) ->
  try
    DistInt = list_to_integer( Dist ),
    Fun = fun() -> kvs:decr( Key, DistInt ) end,
    do_incr_decr_impl( Soc, Fun )
  catch
    error:_Reason ->
      send_error( Soc ),
      {ok, []}
  end;
exec_command_impl( ["stats"], Soc ) ->
  %% 適当Ver
  {_, Sec, _} = now(),
  gen_tcp:send( Soc, io_lib:format( "~s\r\n",    ["STAT version 0.9.0"] ) ),
  gen_tcp:send( Soc, io_lib:format( "~s\r\n",    ["STAT threads 1"] ) ),
  gen_tcp:send( Soc, io_lib:format( "~s ~p\r\n", ["STAT time", Sec] ) ),
  gen_tcp:send( Soc, io_lib:format( "END\r\n" ) ),
  {ok, []};
exec_command_impl( ["quit"], _Soc ) ->
  {close, []};
exec_command_impl( _, Soc) ->
  send_error( Soc ),
  {ok, []}.

%%------------------------------------------------------------------------------------------------
%% implements
%%------------------------------------------------------------------------------------------------
send_error( Soc ) ->
  gen_tcp:send( Soc, "ERROR\r\n" ).

do_incr_decr_impl( Soc, Fun ) ->
  case Fun() of
    {ok, Result} ->
      Value = list_to_integer( Result ),
      gen_tcp:send( Soc, io_lib:format( "~w\r\n", [Value] ) );
    _ ->
      send_error( Soc )
  end.

read_value( Soc, Len, _Buf, Callback ) ->
  try
    inet:setopts( Soc, [{packet, raw}] ),
    case gen_tcp:recv( Soc, Len ) of
      {ok, Result} ->
        Value = binary_to_list( Result ),
        Size = length( Value ),
        if
          Size =/= Len ->
            R = {error, less};
          true ->
            Callback( Value ),
            R = Value
        end,
        {ok, R};

      Other ->
        erlang:error( Other )
    end
  after
    inet:setopts( Soc, [{packet, line}] )
  end.