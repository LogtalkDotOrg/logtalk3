%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura and Daniel L. Dudley',
		date is 2016/03/12,
		comment is 'Unit tests for the iso8601 library.'
	]).

	:- uses(iso8601, [
		date/4, date/5, date/6, date/7,
		date_string/3,
		valid_date/3,
		leap_year/1, calendar_month/3, easter_day/3
	]).

	% date/4 tests

	- test(iso8601_date_4_01) :-
		% Current date (i.e., today)
		date(JD, Y, M, D),
		JD == 2453471, Y == 2005, M == 4, D == 10.

	test(iso8601_date_4_02) :-
		% Convert a date to its Julian day number
		date(JD, 2000, 2, 29),
		JD == 2451604.

	test(iso8601_date_4_03) :-
		% Convert a Julian day number to its date
		date(2451604, Yr, Mth, Day),
		Yr == 2000, Mth == 2, Day == 29.

	test(iso8601_date_4_04) :-
		% What is the date of day # 60 in year 2000?
		date(J, 2000, 1, 60),
		J == 2451604.

	test(iso8601_date_4_05) :-
		% What is the Julian of the 1st day prior to 2000-1-1?
		date(J, 2000, 1, 0),
		J == 2451544.

	test(iso8601_date_4_06) :-
		% What is the Julian of the 60th day prior to 2000-1-1?
		date(J, 2000, 1, -59),
		J == 2451485.

	test(iso8601_date_4_07) :-
		% Illegal date is auto-adjusted (see also next query)
		date(JD, 1900, 2, 29),
		JD == 2415080.

	test(iso8601_date_4_08) :-
		% This is the correct date!
		date(2415080, Y, M, D),
		Y = 1900, M = 3, D = 1.

	% date/5 tests

	test(iso8601_date_5_01) :-
		% Get the Julian and the day-of-week # of a date
		date(JD, 2000, 2, 29, DoW),
		JD == 2451604, DoW == 2.

	test(iso8601_date_5_02) :-
		% Check the validity of a given date (day-of-week is 2, not 4)
		\+ date(_, 2002, 3, 5, 4).

	test(iso8601_date_5_03) :-
		% Get the Julian day of a given date if it is a Sunday
		date(JD, 2004, 2, 29, 7),
		JD == 2453065.

	test(iso8601_date_5_04) :-
		% Get the date and day-of-week # of a Julian
		date(2451545, Y, M, D, DoW),
		Y == 2000, M == 1, D == 1, DoW == 6.

	% date/6 tests

	test(iso8601_date_6_01) :-
		% Get the day-of-week and week number of a date
		date(_, 2000, 1, 1, DoW, Wk),
		DoW == 6, Wk == week(52,1999).

	- test(iso8601_date_6_02) :-
		% Get the week number and year of this week
		date(_, _, _, _, _, Wk),
		Wk == week(7, 2004).

	test(iso8601_date_6_03) :-
		% Get the Julian number and the week of a date if it is a Sunday
		date(JD, 2004, 2, 29, 7, Wk),
		JD == 2453065, Wk == week(9,2004).

	test(iso8601_date_6_04) :-
		% Get the day-of-week and week of a Julian day number
		date(2453066, _, _, _, DoW, Wk),
		DoW == 1, Wk == week(10,2004).

	test(iso8601_date_6_05) :-
		% Check that given date data matches
		date(_, 2004, 3, 1, 1, week(10,2004)).

	test(iso8601_date_6_06) :-
		% What is the date of a day of week (default is 1) in given week # and year?
		date(_, Y, M, D, DoW, week(26,2004)),
		Y == 2004, M == 6, D == 21, DoW == 1.

	test(iso8601_date_6_07) :-
		% Ditto for Sunday
		date(_, Y, M, D, 7, week(1,2005)),
		Y == 2005, M == 1, D == 9.

	test(iso8601_date_6_08) :-
		% Ditto for Tuesday in following week
		date(_, Y, M, D, 9, week(1,2005)),
		Y == 2005, M == 1, D == 11.
		
	test(iso8601_date_6_09) :-
		% Ditto for Thursday in the prior week
		date(_, Y, M, D, 4, week(0,2005)),
		Y == 2004, M == 12, D == 30.

	test(iso8601_date_6_10) :-
		% Ditto for Tuesday two weeks prior
		date(_, Y, M, D, 2, week(-1,2005)),
		Y == 2004, M == 12, D == 21.

	test(iso8601_date_6_11) :-
		% Ditto for Saturday
		date(_, Y, M, D, 6, week(53,2004)),
		Y == 2005, M == 1, D == 1.
		
	test(iso8601_date_6_12) :-
		% Ditto for Monday (note automatic compensation of nonexistent week number)
		date(_, Y, M, D, 1, week(60,2004)),
		Y == 2005, M == 2, D == 14.

	% date/7 tests

	test(iso8601_date_7_01) :-
		% Get the date and day-of-year of a Julian number
		date(2451649, Year, Month, Day, _, _, DoY),
		Year == 2000, Month == 4, Day == 14, DoY == 105.
		
	test(iso8601_date_7_02) :-
		% Get the Julian number, week number and day-of-year of a date, confirming that it is a Sunday
		date(JD, 2004, 2, 29, 7, Wk, DoY),
		JD == 2453065, Wk == week(9,2004), DoY == 60.

	test(iso8601_date_7_03) :-
		% Confirm that a date is, in fact, a specific day-of-year
		date(_, 2004, 3, 1, _, _, 61).

	test(iso8601_date_7_04) :-
		% Get the Julian number, week day and day-of-year of a date
		date(JD, 2004, 10, 18, DoW, _, DoY),
		JD == 2453297, DoW == 1, DoY == 292.

	- test(iso8601_date_7_05) :-
		% Get today''s day-of-year
		date(_, _, _, _, _, _, DoY),
		DoY == 54.

	test(iso8601_date_7_06) :-
		% Get all missing date data (excl. Julian number) for the 60th calendar day of 2004
		date(_, 2004, Month, Day, DoW, Week, 60),
		Month == 2, Day == 29, DoW == 7, Week == week(9,2004).

	test(iso8601_date_7_07) :-
		% Match given date data and, if true, return the missing data (excl. Julian number)
		date(_, 2004, 3, Day, DoW, Week, 61),
		Day == 1, DoW == 1, Week == week(10,2004).
		
	test(iso8601_date_7_08) :-
		% Ditto (the 61st day-of-year cannot be both day 1 and 2 of the month)
		\+ date(_, 2004, _Month, 2, _DoW, _Week, 61).

	% date_string/3 tests

	test(iso8601_date_string_3_01) :-
		% Date, complete, basic (section 5.2.1.1)
		date_string('YYYYMMDD', [2004,2,29], Str),
		Str == '20040229'.

	test(iso8601_date_string_3_02) :-
		% Date, complete, basic (section 5.2.1.1)
		date_string('YYYYMMDD', Day, '20040229'),
		Day == [2004,2,29].

	test(iso8601_date_string_3_03) :-
		% Date, complete, extended (section 5.2.1.1)
		date_string('YYYY-MM-DD', [2003,12,16], Str),
		Str == '2003-12-16'.

	test(iso8601_date_string_3_04) :-
		% Date, complete, extended (section 5.2.1.1)
		date_string('YYYY-MM-DD', Day, '2003-12-16'),
		Day == [2003,12,16].

	- test(iso8601_date_string_3_05) :-
		% Date, complete, extended (section 5.2.1.1)
		date_string('YYYY-MM-DD', _, Str),
		Str == '2004-02-17'.

	test(iso8601_date_string_3_06) :-
		% Date, complete, extended (section 5.2.1.1)
		date_string('YYYY-MM-DD', Day, '2004-02-17'),
		Day == [2004,2,17].

	test(iso8601_date_string_3_07) :-
		% Date, reduced, month (section 5.2.1.2 a)
		date_string('YYYY-MM',[2004,9,18],Str),
		Str == '2004-09'.

	test(iso8601_date_string_3_08) :-
		% Date, reduced, month (section 5.2.1.2 a)
		date_string('YYYY-MM',Day,'2004-09'),
		Day == [2004,9].

	test(iso8601_date_string_3_09) :-
		% Date, reduced, year (section 5.2.1.2 b)
		date_string('YYYY', [1900,7,24], Str),
		Str == '1900'.

	test(iso8601_date_string_3_10) :-
		% Date, reduced, year (section 5.2.1.2 b)
		date_string('YYYY', Day, '1900'),
		Day == [1900].

	test(iso8601_date_string_3_11) :-
		% Date, reduced, century (section 5.2.1.2 c)
		date_string('YY',2456557,Str),
		Str == '20'.

	test(iso8601_date_string_3_12) :-
		% Date, reduced, century (section 5.2.1.2 c)
		date_string('YY', Day, '20'),
		Day == [20].

	test(iso8601_date_string_3_13) :-
		% Date, ordinal, complete (section 5.2.2.1)
		date_string('YYYYDDD', [2005,3,25], Str),
		Str == '2005084'.

	test(iso8601_date_string_3_14) :-
		% Date, ordinal, complete (section 5.2.2.1)
		date_string('YYYYDDD', Day, '2005084'),
		Day == [2005,84].

	test(iso8601_date_string_3_15) :-
		% Date, ordinal, extended (section 5.2.2.1)
		date_string('YYYY-DDD', [1854,12,4], Str),
		Str == '1854-338'.

	test(iso8601_date_string_3_16) :-
		% Date, ordinal, extended (section 5.2.2.1)
		date_string('YYYY-DDD', Day, '1854-338'),
		Day == [1854,338].

	test(iso8601_date_string_3_17) :-
		% Week, complete, basic (section 5.2.3.1)
		date_string('YYYYWwwD', [2000,1,2], Str),
		Str == '1999W527'.

	test(iso8601_date_string_3_18) :-
		% Week, complete, basic (section 5.2.3.1)
		date_string('YYYYWwwD', Day, '1999W527'),
		Day == [1999,52,7].

	test(iso8601_date_string_3_19) :-
		% Week, complete, extended (section 5.2.3.1)
		date_string('YYYY-Www-D', [2003,12,29], Str),
		Str == '2004-W01-1'.

	test(iso8601_date_string_3_20) :-
		% Week, complete, extended (section 5.2.3.1)
		date_string('YYYY-Www-D', Day, '2004-W01-1'),
		Day == [2004,1,1].

	test(iso8601_date_string_3_21) :-
		% Week, complete, extended (section 5.2.3.1)
		date_string('YYYY-Www-D', 2453167, Str),
		Str == '2004-W24-4'.

	test(iso8601_date_string_3_22) :-
		% Week, complete, extended (section 5.2.3.1)
		date_string('YYYY-Www-D', Day, '2004-W24-4'),
		Day == [2004,24,4].

	test(iso8601_date_string_3_23) :-
		% Week, reduced, basic (section 5.2.3.2)
		date_string('YYYYWww', [2004,2,29], Str),
		Str == '2004W09'.

	test(iso8601_date_string_3_24) :-
		% Week, reduced, basic (section 5.2.3.2)
		date_string('YYYYWww', Day, '2004W09'),
		Day == [2004,9].

	test(iso8601_date_string_3_25) :-
		% Week, reduced, extended (section 5.2.3.2)
		date_string('YYYY-Www', [2004,2,29], Str),
		Str == '2004-W09'.

	test(iso8601_date_string_3_26) :-
		% Week, reduced, extended (section 5.2.3.2)
		date_string('YYYY-Www', Day, '2004-W09'),
		Day == [2004,9].

	% valid_date/3 tests

	test(iso8601_valid_date_3_01) :-
		% Yes, the recent millenium was a leap year
		valid_date(2000, 2, 29).

	test(iso8601_valid_date_3_02) :-
		% 2004 was also a leap year
		valid_date(2004, 2, 29).

	test(iso8601_valid_date_3_03) :-
		% Only 30 days in April
		\+ valid_date(2004, 4, 31).

	test(iso8601_valid_date_3_04) :-
		% 1 BC was a leap year
		valid_date(-1, 2, 29).

	% leap_year/1

	test(iso8601_leap_year_1_01) :-
		% No, the prior centenary was not a leap year
		\+ leap_year(1900).

	test(iso8601_leap_year_1_02) :-
		% The recent millenium
		leap_year(2000).

	- test(iso8601_leap_year_1_03) :-
		% This year
		leap_year(Year),
		Year == 2004.

	- test(iso8601_leap_year_1_04) :-
		% This year (equivalent to prior query)
		leap_year(_).

	test(iso8601_leap_year_1_05) :-
		% Next centennial
		\+ leap_year(2100).

	test(iso8601_leap_year_1_06) :-
		% Year 0, equivalent to 1 BC
		leap_year(0).

	test(iso8601_leap_year_1_07) :-
		% 1 BC
		leap_year(-1).

	test(iso8601_leap_year_1_08) :-
		% 4 BC
		\+ leap_year(-4).

	test(iso8601_leap_year_1_09) :-
		% 5 BC
		leap_year(-5).

	% calendar_month/3 tests

	test(iso8601_calendar_month_3_01) :-
		% Compute the calendar of March, 2005
		calendar_month(2005, 3, Calendar),
		Calendar == m(2005, 3, [
			w( 9, [ 0,  1,  2,  3,  4,  5,  6]),
			w(10, [ 7,  8,  9, 10, 11, 12, 13]),
			w(11, [14, 15, 16, 17, 18, 19, 20]),
			w(12, [21, 22, 23, 24, 25, 26, 27]),
			w(13, [28, 29, 30, 31,  0,  0,  0]),
			w( 0, [ 0,  0,  0,  0,  0,  0,  0])
		]).

	% easter_day/3 tests

	test(iso8601_easter_day_3_01) :-
		% Compute Easter Sunday for a particular year
		easter_day(2006, Month, Day),
		Month == 4, Day == 16.

	- test(iso8601_easter_day_3_02) :-
		% Compute Easter Sunday for the current year
		easter_day(Year, Month, Day),
		Year == 2005, Month == 3, Day == 27.

:- end_object.
