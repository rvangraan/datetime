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
	 to_string/1,
	 to_datetime/1,
	 utc_time/1
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


new(DateTime) ->
    to_datetime(DateTime).
%%--------------------------------------------------------------------------------------------------

-spec new(t_date(),t_time()) -> #datetime{}.       

new(Date,Time) when is_tuple(Date), is_tuple(Time) ->
  #datetime{ date = date:new(Date), time = time:new(Time) }.
  

date_test_() ->
  [
    ?_assertMatch(
      #datetime{ date = #date{ y=_Y, m=_MM, d=_DD}, time = #time{ h=_H, m=_M, s=_SS} },
      new()),
    ?_assertMatch(
      #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1} },
      new({2001, 11, 12}, {10, 5, 1})),
    ?_assertMatch(
      #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1, tz="SAST"} },
      new("2001/11/12 10:05:01 SAST")),
   ?_assertMatch(
      #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1, tz="SAST"} },
      new("2001-11-12 10:05:01 SAST")),
   ?_assertMatch(
      #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1, tz="SAST"} },
      new("2001-11-12T10:05:01SAST"))
  ].

%%--------------------------------------------------------------------------------------------------

-spec to_string(#datetime{}) -> string().

%% @doc
%% Converts #datetime{} to an string using formating '{YYYY}-{MM}-{DD}T{HH}:{MM}:{SS}'
%% @enc

to_string(DateTime) when is_record(DateTime, datetime) ->
    Time = #time{tz=TimeZone} = DateTime#datetime.time,
    Date = #date{}            = DateTime#datetime.date,
    Format = "Y-m-d H:i:s T",
    qdate:to_string(Format,TimeZone,{{date:to_erlang(Date),time:to_erlang(Time)},TimeZone}).

to_string_test_() ->
    [
     ?_assertEqual( "2001-11-12 10:05:01 GMT", to_string(#datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1} })),
     ?_assertEqual( "2001-11-12 10:05:01 SAST", to_string(#datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1, tz="SAST"} })),
     ?_assertEqual( "2013-07-06 01:00:00 TKT", to_string(#datetime{date = #date{ y=2013, m=7, d=6}, time = #time{ h=1, m=0, s=0, tz="TKT"}} ))
     ].
  
%%--------------------------------------------------------------------------------------------------

to_datetime(DateTime) ->
    Disamb = prefer_standard,
    DateTimeInfo = qdate:datetime_info(Disamb,DateTime),
    #datetime{ date = date:new(DateTimeInfo), time = time:new(DateTimeInfo)}.

%% application:set_env(qdate, deterministic_parsing, {now, zero}).


utc_time(DateTime = #datetime{date = #date{} = Date, time=#time{tz=TZ} = Time}) ->
    ErlangDateTimeWithZone = {{date:to_erlang(Date),time:to_erlang(Time)},TZ},
    {UTCDate, UTCTime} = qdate:to_date("UTC",ErlangDateTimeWithZone),
    DateTime#datetime{date = date:new(UTCDate), time = time:new(UTCTime,"UTC")}.
	    
utc_time_test_() ->
    [
     ?_assertEqual( #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1, tz="UTC"}}, 
		    utc_time(#datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1}})),
     ?_assertEqual( #datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=8, m=5, s=1, tz="UTC"}}, 
		    utc_time(#datetime{date = #date{ y=2001, m=11, d=12}, time = #time{ h=10, m=5, s=1, tz="SAST"}})),
     ?_assertEqual( #datetime{date = #date{ y=2013, m=3, d=7}, time = #time{ h=7, m=0, s=0, tz="UTC"}}, 
		    utc_time(#datetime{date = #date{ y=2013, m=3, d=7}, time = #time{ h=1, m=0, s=0, tz="CST"}})),
     ?_assertEqual( #datetime{date = #date{ y=2013, m=6, d=7}, time = #time{ h=6, m=0, s=0, tz="UTC"}}, 
		    utc_time(#datetime{date = #date{ y=2013, m=6, d=7}, time = #time{ h=1, m=0, s=0, tz="CST"}})),
     ?_assertEqual( #datetime{date = #date{ y=2013, m=6, d=7}, time = #time{ h=1, m=0, s=0, tz="UTC"}}, 
		    utc_time(#datetime{date = #date{ y=2013, m=6, d=7}, time = #time{ h=1, m=0, s=0, tz="GMT"}})),
     
     %% Saturday, July 6, 2013 at 1:00:00 AM	TKT	UTC+13 hours
     %% Friday, July 5, 2013 at 12:00:00        GMT     UTC
     ?_assertEqual( #datetime{date = #date{ y=2013, m=7, d=5}, time = #time{ h=12, m=0, s=0, tz="UTC"}}, 
		    utc_time(#datetime{date = #date{ y=2013, m=7, d=6}, time = #time{ h=1, m=0, s=0, tz="TKT"}}))

    ].
