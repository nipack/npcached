%% Copyright
-module(test).
-include_lib( "eunit/include/eunit.hrl" ).
-include_lib( "stdlib/include/qlc.hrl" ).
-include("values.hrl").

-author("nipack").

%% API
-export([]).

%%----------------------------------------------------------------------------------------------------------------------
%% Test case
%%----------------------------------------------------------------------------------------------------------------------
init() ->
  mnesia:start(),
  case mnesia:create_table( values,
    [{attributes, record_info(fields, values)}]) of
    {atomic, ok} ->
      ok;
    _ ->
      error
  end.

get_value_test() ->
  init(),
  ?assertEqual( {notfound, []}, database:get_value( "test" ) ).

set_value_test() ->
  ?assertEqual( {ok, []}, database:set_value( "test", "test", 0, 0 ) ),
  ?assertEqual( {ok, "test"}, database:get_value( "test" ) ).

delete_value_test() ->
  ?assertEqual( {ok, []}, database:set_value( "test", "test", 0, 0 ) ),
  ?assertEqual( {ok, []}, database:delete_value( "test" ) ),
  ?assertEqual( {notfound, []}, database:get_value( "test" ) ).

set_value_simple_test() ->
  ?assertEqual( {ok, []}, database:set_value( "test", "test", 0, 0 ) ),
  ?assertEqual( {ok, "example"}, database:set_value_simple( "test", "example" ) ),
  ?assertEqual( {ok, "example"}, database:get_value( "test" ) ).

incr_value_test() ->
  Key = "test",
  ?assertEqual( {ok, []}, database:set_value( Key, "1", 0, 0 ) ),
  ?assertEqual( {ok, "2"}, database:set_value_simple( Key, "2" ) ),
  Result = database:get_value( Key ),
  ?assertEqual( {ok, "2"}, Result ),
  case Result of
    {ok, Value} ->
      V = list_to_integer( Value ),
      R = database:set_value_simple( Key, V + 1 );
    _Other ->
      R = {notfound, []}
  end,
  ?assertEqual( {ok, 3}, R ).

do_sweep_test() ->
  ?assertEqual( {ok, []}, database:set_value( "test", "test", 0, 0 ) ),
  ?assertEqual( {ok, "test"}, database:get_value( "test" ) ),
  ?assertEqual( {ok, 0}, database:do_sweep() ),
  ?assertEqual( {ok, []}, database:set_value( "test1", "test", 0, 1 ) ),
  ?assertEqual( {ok, []}, database:set_value( "test2", "test", 0, 5 ) ),
  receive
  after 2000 ->
    ?assertEqual( {ok, 1}, database:do_sweep() ),
    ?assertEqual( {ok, "test"},   database:get_value( "test" ) ),
    ?assertEqual( {notfound, []}, database:get_value( "test1" ) ),
    ?assertEqual( {ok, "test"},   database:get_value( "test2" ) )
  end,
  mnesia:stop().