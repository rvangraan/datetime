%%--------------------------------------------------------------------------------------------------
-module(time).
%%--------------------------------------------------------------------------------------------------
-include_lib("datetime/include/datetime.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  new/0,
  new/1,
  new/2,
  new/3,
  new/4,
  new/5,
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

new(Time = #time{}) ->
  Time;

new(_Time = {Hour, Minute, Second}) ->
  new(Hour, Minute, Second).

%%--------------------------------------------------------------------------------------------------

-spec new(t_time(), offset()) -> #time{}.
       
new(_Time = {Hour, Minute, Second}, Offset) ->
  new(Hour, Minute, Second, Offset).

%%--------------------------------------------------------------------------------------------------

-spec new(hour(), minute(), second()) -> #time{};
         (t_time(), offset(), tz())   -> #time{}.

new(Hour,Minute,Second) 
when 
  is_integer(Hour), Hour >= 0, Hour =< 23,
  is_integer(Minute), Minute >= 0, Minute =< 59,
  is_integer(Second), Second >= 0, Second =< 59 
->
  #time{m = Minute, h = Hour, s = Second, offset = local};

new({Hour,Minute,Second},Offset,TimeZone) 
when 
  is_integer(Hour), Hour >= 0, Hour =< 23,
  is_integer(Minute), Minute >= 0, Minute =< 59,
  is_integer(Second), Second >= 0, Second =< 59,
  ((is_integer(Offset) and (Offset >= -12) and (Offset =< 12)) or (Offset =:= local)),
  (is_list(TimeZone) or (TimeZone =:= undefined))
->
  new(Hour, Minute, Second, Offset, TimeZone).

%%--------------------------------------------------------------------------------------------------  

-spec new( hour(), minute(), second(), offset()) -> #time{}.

new(Hour,Minute,Second,Offset) 
when 
  is_integer(Hour), Hour >= 0, Hour =< 23,
  is_integer(Minute), Minute >= 0, Minute =< 59,
  is_integer(Second), Second >= 0, Second =< 59,
  ((is_integer(Offset) and (Offset >= -12) and (Offset =< 12)) or (Offset =:= local)) 
->
  new(Hour, Minute, Second, Offset, undefined).

%%--------------------------------------------------------------------------------------------------

-spec new(hour(), minute(), second(), offset(), tz()) -> #time{}.

new(Hour, Minute, Second, 0, undefined) ->
  new(Hour, Minute, Second, 0, ?UTC);

new(Hour, Minute, Second, Offset, TimeZone) 
when 
  is_integer(Hour), Hour >= 0, Hour =< 23,
  is_integer(Minute), Minute >= 0, Minute =< 59,
  is_integer(Second), Second >= 0, Second =< 59,
  ((is_integer(Offset) and (Offset >= -12) and (Offset =< 12)) or (Offset =:= local)),
  (is_list(TimeZone) or (TimeZone =:= undefined))
->
  #time{m = Minute, h = Hour, s = Second, offset = Offset, tz = TimeZone}.

%%--------------------------------------------------------------------------------------------------

-spec to_string( #time{} ) -> string().

to_string(Time) ->
  {time, Hour, Min, Sec, _Milisec, _, _} = Time, 
  lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B",[Hour, Min, Sec])).        

to_string_test() ->
  Time = {time, 12, 18, 46, 0, local, undefined},
  ?assertEqual( "12:18:46", to_string(Time) ).

%%--------------------------------------------------------------------------------------------------

-spec to_erlang( #time{} ) -> t_time(). 

to_erlang(Time) ->
  {time, Hour, Min, Sec, _Milisec, _, _} = Time, 
  {Hour, Min, Sec}.

to_erlang_test() ->
  Time = {time, 12, 18, 46, 0, local, undefined},
  ?assertEqual( {12, 18, 46}, to_erlang(Time) ).

%%--------------------------------------------------------------------------------------------------

new_test_() ->
  [
    ?_assertMatch(
      #time{ h=10, m=11, s=12, offset=local },
      new(10, 11, 12)),
    ?_assertMatch(
      #time{ h=10, m=11, s=12, offset=local },
      new({10, 11, 12})),
    ?_assertMatch(
      #time{ h=10, m=11, s=12, offset=2, tz=?UTC },
      new({10, 11, 12}, 2, ?UTC))
  ].

%%--------------------------------------------------------------------------------------------------
  
%% @doc Returns the time() representation of the timestamp Now.
-spec from_now( now() ) -> #time{}.

from_now(Now) ->
  {_Date, {HH, MM, SS}} = calendar:now_to_datetime(Now),
  #time{m = MM, h = HH, s = SS}.

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