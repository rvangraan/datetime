%%--------------------------------------------------------------------------------------------------
-module(time).
%%--------------------------------------------------------------------------------------------------
-include("../include/datetime.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  new/0,
  new/1,
	 new/2,
	 new/3,
  to_string/1,
  to_erlang/1,
  from_now/1
]).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns the time() representation of now().
-spec new() -> #time{}.

new() ->
  from_now( erlang:now() ).

%%--------------------------------------------------------------------------------------------------

-spec new(t_time()) -> #time{};
         (t_time()) -> #time{}.

base_time_data() ->
    #{tz=>"GMT"}.

new(DateTimeInfo = #{time := Time}) when is_tuple(Time)->
    #{tz := TimeZone} = maps:merge(base_time_data(),DateTimeInfo),
    new(Time,TimeZone);

new(Time = #time{}) ->
  Time;

new(_Time = {Hour, Minute, Second}) ->
  new(Hour, Minute, Second).


%%--------------------------------------------------------------------------------------------------
-spec new({hour(), minute(), second()},tz()) -> #time{}.

new({Hour,Minute,Second},TimeZone) when  is_integer(Hour), Hour >= 0, Hour =< 23,
					 is_integer(Minute), Minute >= 0, Minute =< 59,
					 is_integer(Second), Second >= 0, Second =< 59,
					 
					 (is_list(TimeZone) or (TimeZone =:= undefined)) ->
    new(Hour, Minute, Second, TimeZone).


%%--------------------------------------------------------------------------------------------------

-spec new(hour(), minute(), second()) -> #time{}.

new(Hour,Minute,Second) when is_integer(Hour), Hour >= 0, Hour =< 23,
			     is_integer(Minute), Minute >= 0, Minute =< 59,
			     is_integer(Second), Second >= 0, Second =< 59 ->
    #time{m = Minute, h = Hour, s = Second,  tz= "GMT"}.


%%--------------------------------------------------------------------------------------------------  

%%--------------------------------------------------------------------------------------------------

-spec new(hour(), minute(), second(), tz()) -> #time{}.

new(Hour, Minute, Second, undefined) ->
  new(Hour, Minute, Second, ?UTC);

new(Hour, Minute, Second, TimeZone) when is_integer(Hour), Hour >= 0, Hour =< 23,
					 is_integer(Minute), Minute >= 0, Minute =< 59,
					 is_integer(Second), Second >= 0, Second =< 59,
					 (is_list(TimeZone) or (TimeZone =:= undefined)) ->
  #time{m = Minute, h = Hour, s = Second, tz = TimeZone}.

%%--------------------------------------------------------------------------------------------------

-spec to_string( #time{} ) -> string().

to_string(Time) ->
  #time{h=Hour, m=Min, s=Sec,tz=_TZ} = Time, 
  lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B",[Hour, Min, Sec])).        

to_string_test() ->
    
  Time = #time{h= 12,m= 18, s=46, tz="GMT"},
  ?assertEqual( "12:18:46", to_string(Time) ).

%%--------------------------------------------------------------------------------------------------

-spec to_erlang( #time{} ) -> t_time(). 

to_erlang(Time) ->
  #time{h= Hour, m=Min, s=Sec} = Time, 
  {Hour, Min, Sec}.

to_erlang_test() ->
  Time = #time{h= 12, m=18, s=46},
  ?assertEqual( {12, 18, 46}, to_erlang(Time) ).

%%--------------------------------------------------------------------------------------------------

new_test_() ->
  [
    ?_assertMatch(
      #time{ h=10, m=11, s=12,  tz="GMT" },
      new(10, 11, 12)),
    ?_assertMatch(
      #time{ h=10, m=11, s=12, tz="GMT" },
      new({10, 11, 12})),
    ?_assertMatch(
      #time{ h=10, m=11, s=12, tz=?UTC },
      new({10, 11, 12}, ?UTC)),
    ?_assertMatch(
      #time{ h=10, m=11, s=12, tz="SAST" },
      new(#{time=>{10, 11, 12}, tz=>"SAST"}))

  ].

%%--------------------------------------------------------------------------------------------------
  
%% @doc Returns the time() representation of the timestamp Now.
-spec from_now( now() ) -> #time{}.

from_now(Now) ->
  {_Date, {HH, MM, SS}} = calendar:now_to_datetime(Now),
  #time{m = MM, h = HH, s = SS, tz = "GMT"}.

time_test_() ->
  [
    ?_assertMatch(
      #time{ h=_H, m=_M, s=_S },
      new()),
    ?_assertMatch( 
      #time{ h=11, m=36, s=48 },
      from_now({1308, 310608, 963227}))
  ].

%%--------------------------------------------------------------------------------------------------
