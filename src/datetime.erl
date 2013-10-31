%%--------------------------------------------------------------------------------------------------
-module(datetime).
%%--------------------------------------------------------------------------------------------------
-include("../include/datetime.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  new/0,
  new/1,
  new/2,
  now/0,
  to_string/1
]).
%%--------------------------------------------------------------------------------------------------
-export_type([r_datetime/0]).
%%--------------------------------------------------------------------------------------------------

now() ->
  new().

%%--------------------------------------------------------------------------------------------------

%% @doc Returns the datetime() representation of now().
-spec new() -> #datetime{}.
        
new() ->
  {Date,Time} = calendar:now_to_datetime( erlang:now() ),
  new(Date, Time).

new(_DateTime = {Date, Time}) ->
  new(Date, Time).

%%--------------------------------------------------------------------------------------------------

-spec new(t_date(),t_time()) -> #datetime{}.       

new(Date,Time) ->
  #datetime{ date = date:new(Date), time = time:new(Time) }.
  

date_test_() ->
  [
    ?_assertMatch(
      #datetime{ date = #date{ y=_Y, m=_MM, d=_DD}, time = #time{ h=_H, m=_M, s=_SS} },
      new()),
    ?_assertMatch(
      #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1} },
      new({2001, 11, 12}, {10, 5, 1}))
  ].

%%--------------------------------------------------------------------------------------------------

-spec to_string(#datetime{}) -> string().

%% @doc
%% Converts #datetime{} to an string using formating '{YYYY}-{MM}-{DD}T{HH}:{MM}:{SS}'
%% @enc

to_string(DateTime) when is_record(DateTime, datetime) ->
  DateStr = date:to_string(DateTime#datetime.date),
  TimeStr = time:to_string(DateTime#datetime.time),
  DateStr ++ "T" ++ TimeStr.

to_string_test() ->
  DateTime = #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1} },
  ?assertEqual( "2001-11-12T10:05:01", to_string(DateTime)).
  
%%--------------------------------------------------------------------------------------------------

