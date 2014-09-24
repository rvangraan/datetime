%%--------------------------------------------------------------------------------------------------
-module(date).
%%--------------------------------------------------------------------------------------------------
-export([
  new/0,
  new/1,
  new/3,
  to_string/1,
  to_erlang/1
]).
%%--------------------------------------------------------------------------------------------------
-include("../include/datetime.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------

-spec new() -> #date{}.

new() ->
  {Year, Month, Day} = date(),
  #date{y = Year, d = Day, m = Month}.

%%--------------------------------------------------------------------------------------------------

-spec new(t_date()) -> #date{};
         (#date{})  -> #date{}.

new(_Date = {Year, Month, Day}) ->
  new(Year, Month, Day);

new(#date{} = Date) ->
    Date;
new(#{date := Date = {_Year, _Month, _Day}}) ->
    new(Date).


%%--------------------------------------------------------------------------------------------------

-spec new(year(), month(), day()) -> #date{}.

new(Year, Month, Day) ->
  #date{y = Year, d = Day, m = Month}.

%%--------------------------------------------------------------------------------------------------

-spec to_string(#date{}) -> string().

to_string(Date) ->
  {date, Year, Month, Day} = Date, 
  lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B",[Year, Month, Day])).


to_string_test() ->
  Date = {date, 2011, 8, 15},
  ?assertEqual( "2011-08-15", to_string(Date) ).

%%--------------------------------------------------------------------------------------------------

-spec to_erlang(#date{}) -> t_date().

to_erlang(Date) ->
  {date, Year, Month, Day} = Date,
  {Year, Month, Day}.

to_erlang_test() ->
  Date = {date, 2011, 11, 11},
  ?assertEqual( {2011, 11, 11}, to_erlang(Date) ).

%%--------------------------------------------------------------------------------------------------

date_test_() ->
  [
    ?_assertMatch(#date{ y=_Y, m=_M, d=_D }, new()),
    ?_assertMatch(#date{ y=2001, m=11, d=12 }, new(2001, 11,12 )),
    ?_assertMatch(#date{ y=2001, m=11, d=12 }, new({2001, 11, 12}))
  ].

%%--------------------------------------------------------------------------------------------------
