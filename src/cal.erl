%%--------------------------------------------------------------------------------------------------
-module(cal).
%%--------------------------------------------------------------------------------------------------
-include("../include/datetime.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  date_to_gregorian_days/1, 
  date_to_gregorian_days/3,
  datetime_to_gregorian_seconds/1,
  day_of_the_week/1,
  day_of_the_week/3,
  gregorian_days_to_date/1,
  gregorian_seconds_to_datetime/1,
  is_leap_year/1,
  last_day_of_the_month/2,
  local_time/0, 
  local_time_to_universal_time/2, 
  local_time_to_universal_time_dst/1, 
  now_to_datetime/1,
  now_to_local_time/1,
  now_to_universal_time/1,
  seconds_to_daystime/1,
  seconds_to_time/1,
  time_difference/2,
  time_to_seconds/1,
  universal_time/0,
  universal_time_to_local_time/1,
  valid_date/1,
  valid_date/3,
  add_seconds_to_timestamp/2,
  seconds_timestamp_difference/2,
  s_timestamp/0,
  ms_timestamp/0
]).
%%--------------------------------------------------------------------------------------------------

-spec date_to_gregorian_days( year(), month(), day()) -> non_neg_integer().

date_to_gregorian_days(Year, Month, Day) when is_integer(Day), Day > 0 ->
  calendar:date_to_gregorian_days(Year, Month, Day).

%%--------------------------------------------------------------------------------------------------

-spec date_to_gregorian_days(t_date())    -> non_neg_integer();
                            (#datetime{}) -> non_neg_integer();
                            (#date{})     -> non_neg_integer().

date_to_gregorian_days(#date{ y=Year, m=Month, d=Day }) ->
  date_to_gregorian_days({Year, Month, Day});

date_to_gregorian_days(#datetime{date = #date{ y=Year, m=Month, d=Day }}) ->
  date_to_gregorian_days({Year, Month, Day});

date_to_gregorian_days({Year, Month, Day}) ->
  date_to_gregorian_days(Year,Month,Day).


date_to_gregorian_days_test_() ->
  [
    ?_assertMatch(730851, date_to_gregorian_days( date:new(2001, 01, 01) )),
    ?_assertMatch(#date{y = 2001, m = 1, d = 1}, gregorian_days_to_date(730851) )
  ].

%%--------------------------------------------------------------------------------------------------

-spec datetime_to_gregorian_seconds(t_datetime()) -> non_neg_integer().

datetime_to_gregorian_seconds(
  #datetime{date=#date{y = Year, m = Month, d = Day}, time=#time{h = Hour, m = Minute, s = Second}}) ->
  datetime_to_gregorian_seconds({ {Year, Month, Day}, {Hour, Minute, Second}});

datetime_to_gregorian_seconds(TS = {_Date, _Time}) ->
  calendar:datetime_to_gregorian_seconds(TS).


datetime_to_gregorian_seconds_test_() ->
  [
    ?_assertMatch(63475535641, 
      datetime_to_gregorian_seconds( datetime:new( date:new(2011, 6, 17), time:new(13, 14, 01) )))
  ].

%%--------------------------------------------------------------------------------------------------

-spec day_of_the_week(t_date()) -> daynum();
                     (#date{})  -> daynum().

day_of_the_week(#datetime{date = #date{ y=Year, m=Month, d=Day }}) ->
  calendar:day_of_the_week({Year, Month, Day});

day_of_the_week({Year, Month, Day}) ->
  calendar:day_of_the_week({Year, Month, Day});

day_of_the_week(#date{ y=Year ,m=Month, d=Day}) ->
  calendar:day_of_the_week({Year, Month, Day}).

%%--------------------------------------------------------------------------------------------------

day_of_the_week(Year, Month, Day) ->
  calendar:day_of_the_week(Year, Month, Day).


day_of_the_week_test_() ->
  [
    ?_assertMatch(3, day_of_the_week(2003, 12, 3)),
    ?_assertMatch(3, day_of_the_week(date:new(2003, 12, 3))),
    ?_assertMatch(3, day_of_the_week(datetime:new( date:new(2003, 12, 3),time:new() )))
  ].

%%--------------------------------------------------------------------------------------------------

-spec gregorian_days_to_date(non_neg_integer()) -> t_date().

gregorian_days_to_date(Days) ->
  {Year, Month, Day} = calendar:gregorian_days_to_date(Days),
  date:new(Year, Month, Day).

gregorian_days_to_date_test_() ->
  [
    ?_assertMatch(#date{y=2001, m=1, d=1}, gregorian_days_to_date(730851))
  ].

%%--------------------------------------------------------------------------------------------------

-spec gregorian_seconds_to_datetime( non_neg_integer() ) -> t_datetime().

gregorian_seconds_to_datetime(Secs) when Secs >= 0 ->
  {Date, Time} = calendar:gregorian_seconds_to_datetime(Secs),
  datetime:new(Date,Time).

gregorian_seconds_to_datetime_test_() ->
  [
    ?_assertMatch(
      #datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} },
      gregorian_seconds_to_datetime( 50876543220 ))
  ].


%%--------------------------------------------------------------------------------------------------

-spec is_leap_year(year()) -> boolean().

is_leap_year(Y) ->
    calendar:is_leap_year1(Y).

%%--------------------------------------------------------------------------------------------------

-spec last_day_of_the_month(year(), month()) -> ldom().

last_day_of_the_month(Y, M) when is_integer(Y), Y >= 0 ->
    calendar:last_day_of_the_month(Y, M).

%%--------------------------------------------------------------------------------------------------

-spec local_time() -> t_datetime().

local_time() ->
  datetime:new( calendar:local_time() ).

local_time_test_() ->
  [
    ?_assertMatch(
       #datetime{ date = #date{}, time = #time{h=_H, m=_M, s=_S, ms=0} },
       local_time())
  ].

%%--------------------------------------------------------------------------------------------------

-spec local_time_to_universal_time(
  t_datetime1970(), 'true' | 'false' | 'undefined') -> t_datetime1970().

local_time_to_universal_time(
  %% TODO: Take timezone into consideration
  #datetime{date = #date{ y=Y, m=M, d=D }, time = #time{ h=HH, m=MM, s=SS} }, IsDst) ->

  {Date,Time} = calendar:local_time_to_universal_time({ {Y,M,D}, {HH,MM,SS}}, IsDst),
  datetime:new(Date,time:new(Time, 0, ?UTC));

local_time_to_universal_time(DateTime = #datetime{date = #date{}, time = #time{ tz=?UTC }}, _IsDst) ->
    %% TODO: Take timezone into consideration
    DateTime;

local_time_to_universal_time(#datetime{date = #date{}, time = #time{ }}, _IsDst) ->
  exit(badarg);

local_time_to_universal_time(DateTime, IsDst) ->
  {Date,Time} = calendar:local_time_to_universal_time(DateTime, IsDst),
  datetime:new(Date,time:new(Time, 0, ?UTC)).

local_time_to_universal_time2_test_() ->
  [
    ?_assertMatch(
      #datetime{date = #date{ y=2001, m=12, d=12}, time = #time{ h=_H, m=_, s=1,  tz=_} },
      local_time_to_universal_time({{2001,12,12},{13,56,1}}, true)),
    ?_assertMatch(
      #datetime{date = #date{ y=2001, m=12, d=12}, time = #time{ h=_H, m=_, s=1, tz=_}},
      local_time_to_universal_time( datetime:new( date:new({2001,12,12}), time:new({13,56,1}, 0)), true)),
    ?_assertExit(
      badarg,
      local_time_to_universal_time(
        #datetime{date = #date{ y=2011, m=06, d=13}, time = #time{ h=14, m=54, s=11, ms=0}}, true) )
  ].

%%--------------------------------------------------------------------------------------------------

-spec universal_time_to_local_time( t_datetime() ) -> t_datetime().

universal_time_to_local_time(DateTime) ->
  calendar:universal_time_to_local_time(DateTime).

%%--------------------------------------------------------------------------------------------------

-spec local_time_to_universal_time_dst( t_datetime1970() ) -> [ t_datetime1970() ].

local_time_to_universal_time_dst(DateTime) ->
  calendar:local_time_to_universal_time_dst(DateTime).

%%--------------------------------------------------------------------------------------------------

-spec now_to_datetime( t_now() ) -> t_datetime1970().

now_to_datetime({MSec, Sec, _uSec}) ->
  {Date, Time} = calendar:now_to_datetime({MSec, Sec, _uSec}),
  datetime:new(Date,time:new(Time, 0, ?UTC)).

now_to_datetime_test_() ->
  [
    ?_assertMatch(
      #datetime{ date = #date{ y=2011, m=6, d=17}, time = #time{ h=16, m=3, s=16,  tz=?UTC} },
      now_to_datetime({1308, 326596, 372064}))
  ].

%%--------------------------------------------------------------------------------------------------

-spec now_to_local_time(t_now()) -> t_datetime1970().

now_to_local_time({MSec, Sec, _uSec}) ->
  {Date,Time} = calendar:now_to_local_time({MSec, Sec, _uSec}),
  datetime:new(Date,time:new(Time, local, undefined)).

now_to_local_time_test_() ->
  [
    ?_assertMatch(
      #datetime{ date = #date{ y=2011, m=6, d=17}, time = #time{ h=_, m=42, s=32,  tz=?UTC} },
      now_to_local_time({1308, 325352, 952886}))
  ].

%%--------------------------------------------------------------------------------------------------

-spec seconds_to_daystime(integer()) -> {integer(), t_time()}.

seconds_to_daystime(Secs) ->
  calendar:seconds_to_daystime(Secs).

%%--------------------------------------------------------------------------------------------------

seconds_to_time(Secs) ->
  time:new(calendar:seconds_to_time(Secs)).

seconds_to_time_test_() ->
  [
    ?_assertMatch( #time{h=_, m=25, s=45, tz=?UTC}, seconds_to_time(12345) )
  ].

%%--------------------------------------------------------------------------------------------------

time_difference(DT1,DT2) ->
  calendar:time_difference(DT1,DT2).

%%--------------------------------------------------------------------------------------------------

time_to_seconds(#time{ h=H, m=M, s=S}) ->
  time_to_seconds({H,M,S});

time_to_seconds(Time) ->
  calendar:time_to_seconds(Time).

time_to_seconds_test_() ->
  [
    ?_assertMatch(11045, time_to_seconds( time:new({3, 4, 5})) ),
    ?_assertMatch(11045, time_to_seconds( {3, 4, 5} ))
  ].

%%--------------------------------------------------------------------------------------------------

-spec now_to_universal_time(t_now()) -> t_datetime1970().

now_to_universal_time(Now) ->
  {Date, _Time = {Hour, Minute, Second}} = calendar:now_to_universal_time(Now),
  datetime:new( Date,time:new(Hour, Minute, Second, 0, ?UTC) ).

now_to_universal_time_test_() ->
  [
    ?_assertMatch( 
      #datetime{ date = #date{ y=2011, m=06, d=17}, time = #time{ h=15, m=15, s=54,  tz=?UTC} },
      now_to_universal_time({1308, 323754, 384823}))   
   ].

%%--------------------------------------------------------------------------------------------------

-spec universal_time() -> t_datetime().

universal_time() ->
  {Date,Time} = calendar:universal_time(),
  datetime:new(Date,time:new(Time, 0, ?UTC)).
  

universal_time_test_() ->
  [
    ?_assertMatch(
      #datetime{date = #date{}, time = #time{}},
      universal_time())
  ].

%%--------------------------------------------------------------------------------------------------

-spec valid_date({integer(), integer(), integer()}) -> boolean();
                (#date{})                           -> boolean().
         
valid_date(#date{ y=Y, m=M, d=D }) ->
  calendar:valid_date({Y, M, D});

valid_date(#datetime{date = #date{ y=Y, m=M, d=D }}) ->
  calendar:valid_date({Y, M, D});

valid_date(Date) ->
  calendar:valid_date(Date).

valid_date_test_() ->
  [
    ?_assertMatch(true, valid_date( date:new() )),
    ?_assertMatch(true, valid_date( datetime:new() ))
  ].

%%--------------------------------------------------------------------------------------------------

-spec valid_date(integer(), integer(), integer()) -> boolean().

valid_date(Y, M, D) ->
  calendar:valid_date(Y, M, D).

%%--------------------------------------------------------------------------------------------------

-spec add_seconds_to_timestamp(#datetime{}, integer()) -> #datetime{}.

add_seconds_to_timestamp(Timestamp, Seconds) ->
    GregorianSeconds = datetime_to_gregorian_seconds(Timestamp) + Seconds,
    gregorian_seconds_to_datetime(GregorianSeconds).

add_seconds_to_timestamp_test_() ->
  [
    ?_assertMatch(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=16,s=0} },
                  add_seconds_to_timestamp(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} }, 540)),
    ?_assertMatch(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=20,m=58,s=0} },
                  add_seconds_to_timestamp(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} }, -540)),
    ?_assertMatch(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} },
                  add_seconds_to_timestamp(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} }, 0))        
  ].

%%--------------------------------------------------------------------------------------------------

-spec seconds_timestamp_difference(#datetime{}, #datetime{}) -> integer().

seconds_timestamp_difference(Timestamp1, Timestamp2) ->
    datetime_to_gregorian_seconds(Timestamp1) - datetime_to_gregorian_seconds(Timestamp2).

seconds_timestamp_difference_test_() ->
    [
     ?_assertMatch(-540,
                   seconds_timestamp_difference(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=20,m=58,s=0} },
                                                #datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} })),
     ?_assertMatch(540,
                   seconds_timestamp_difference(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=16,s=0} },
                                                #datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} })),
     ?_assertMatch(0,
                   seconds_timestamp_difference(#datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} },
                                                #datetime{ date = #date{ y=1612, m=3, d=18}, time = #time{h=21,m=7,s=0} }))                                                       
     ].

s_timestamp() ->
    now_to_seconds(os:timestamp()).

ms_timestamp() ->
    now_to_milliseconds(os:timestamp()).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.   
    
now_to_milliseconds({Mega, Sec, Micro}) ->
    (Mega * 1000000 * 1000) + (Sec * 1000) + (Micro div 1000).
