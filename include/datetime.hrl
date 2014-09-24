%%--------------------------------------------------------------------------------------------------
-ifndef('datetime.hrl').
-define('datetime.hrl', true).
%%--------------------------------------------------------------------------------------------------
-define(UTC,"UTC").
%%--------------------------------------------------------------------------------------------------

-type year()     :: non_neg_integer().
-type year1970() :: 1970..10000.        % should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-type ldom()     :: 28 | 29 | 30 | 31. % last day of month

-type t_now()          :: { non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type t_date()         :: { year(), month(), day()}.
-type t_time()         :: { hour(), minute(), second()}.
-type t_datetime()     :: { t_date(), t_time()}.
-type t_datetime1970() :: { {year1970(), month(), day()}, t_time()}.

-type tz()     :: string() | local.
-type gmt_offset() :: integer() | undefined.
-type now()    :: tuple( integer(), integer(), integer() ).

%%--------------------------------------------------------------------------------------------------

-record(date,{
  y = 0 :: year(),
  m = 0 :: month(),
  d = 0 :: day()
}).

-record(time,{
  h = 0              :: hour(),
  m = 0              :: minute(),
  s = 0              :: second(),
  ms = 0             :: integer(),
  tz = "GMT"         :: tz()
}).

-record(datetime,{
  date :: #date{},
  time :: #time{}
}).

-record(dt_format_spec,
	{format_spec=iso8601,
	 null_behaviour=retain % retain | replace_with_undefined 
	}).
	  

%%--------------------------------------------------------------------------------------------------

-type r_time()     :: #time{}.
-type r_date()     :: #date{}.
-type r_datetime() :: #datetime{}.

%%--------------------------------------------------------------------------------------------------
-endif.
%%--------------------------------------------------------------------------------------------------
