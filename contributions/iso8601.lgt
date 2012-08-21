/******************************************************************************

	Library: ISO8601.PL
	Copyright (c) 2004-2005 Daniel L. Dudley

	Purpose: ISO 8601 (and European civil calendar) compliant library of date
			 and time (clock) related predicates. That is, an ISO 8601 handler.

	Author:  Daniel L. Dudley
	Created: 2004-02-18

******************************************************************************/


:- object(iso8601).

	:- info([
		version is 1.0,
		author is 'Daniel L. Dudley',
		date is 2005/04/11,
		comment is 'ISO 8601 (and European civil calendar) compliant library of date predicates.',
		remarks is [
			'Scope:' - 'This object currently provides a powerful, versatile and efficient set of date-handling predicates, which--thanks to Logtalk--may be used as is on a wide range of Prolog compilers. Besides taking time to familiarize oneself with each predicate, the user should take note of the following information.',
			'Validation of dates:' - 'Date parts are not validated--that is the caller''s responsibility! However, not being quite heartless yet, we do provide a predicate for this purpose.',
			'Date arithmetic:' - 'Many of the examples illustrate a simplified method of doing date arithmetic. Note, however, that we do not generally recommend this practice--it is all too easy to make mistakes. The safest way of finding the day difference between two dates is to first convert the dates to their Julian day numbers and then subtract one from the other. Similarly, the safe way to add or subtract a day offset to a particular date is to first convert the date to its Julian day number, add or subtract the day offset, and then convert the result to its corresponding date.',
			'BC years:' - 'ISO 8601 specifies that the Gregorian calendar be used, yet requires that years prior to 1 AD be handled arithmetically, i.e., the year we know as 1 BC is year 0, 2 BC is year -1, 3 BC is year -2 and so on. We do not follow ISO 8601 with regard to the handling of BC years. Our date predicates will accept and interpret an input year 0 as 1 BC; however, a negative year, Year, should always be interpreted as abs(Year) =:= Year BC. We believe that the average person will find our handling of BC years more user-friendly than the ISO 8601 one, but we encourage feedback from users with a view to a possible change in future versions.',
			'Week numbers:' - 'It is possible for a day (date) to have a week number that belongs to another year. Up to three of the first days of a calendar year may belong to the last week (number) of the prior calendar year, and up to three days of the last days of a calendar year may belong to the first week (number) of the next calendar year. It for this reason that the Week parameter in date/6-7 is a compound term, namely week(WeekNo,ActualYear).',
			'Computation of Gregorian Easter Sunday:' - 'The algorithm is based upon the "Gaussian rule". Proleptic use is limited to years > 1582 AD, that is, after the introduction of the Gregorian calendar.',
			'Some Christian feast day offsets from Easter Sunday:' - 'Carnival Monday: -48 days, Mardi Gras (Shrove Tuesday): -47 days, Ash Wednesday: -46 days, Palm Sunday: -7 days, Easter Friday: -2 days, Easter Saturday: -1 day, Easter Monday: +1 day, Ascension of Christ: +39 days, Whitsunday: +49 days, Whitmonday: +50 days, Feast of Corpus Christi: +60 days.'
			]]).

	% CORE PREDICATES:

	:- public(date/4).
	:- mode(date(?integer, ?integer, ?integer, ?integer), zero_or_one).
	:- info(date/4, [
		comment is 'Get the system date and/or its Julian Day # or convert a Julian Day # to/from given date parts.',
		arguments is [
			'JD' - 'Julian day serial number',
			'Year' - '0 or negative if converted BC year, positive otherwise',
			'Month' - 'Normally an integer between 1 and 12 inclusive',
			'Day' - 'Normally an integer between 1 and 31 inclusive depending upon month'],
		examples is [
			'Current date (i.e., today):' - date(JD,Y,M,D) - {JD = 2453471, Y = 2005, M = 4, D = 10},
			'Convert a date to its Julian day number:' - date(JD,2000,2,29) - {JD = 2451604},
			'Convert a Julian day number to its date:' - date(2451604,Yr,Mth,Day) - {Yr = 2000, Mth = 2, Day = 29},
			'What is the date of day # 60 in year 2000?' - date(J,2000,1,60) - {J = 2451604},
			'What is the Julian of the 1st day prior to 2000-1-1?' - date(J,2000,1,0) - {J = 2451544},
			'What is the Julian of the 60th day prior to 2000-1-1?' - date(J,2000,1,-59) - {J = 2451485},
			'Illegal date is auto-adjusted (see also next query)' - date(JD,1900,2,29) - {JD = 2415080},
			'This is the correct date!' - date(2415080,Y,M,D) - {Y = 1900, M = 3, D = 1}]]).

	:- public(date/5).
	:- mode(date(?integer, ?integer, ?integer, ?integer, ?integer), zero_or_one).
	:- info(date/5, [
		comment is 'Ditto date/4 + get/check its day-of-week #.',
		arguments is [
			'JD' - 'Julian day serial number',
			'Year' - '0 or negative if converted BC year, positive otherwise',
			'Month' - 'Normally an integer between 1 and 12 inclusive',
			'Day' - 'Normally an integer between 1 and 31 inclusive depending upon month',
			'DoW' - 'Day of week, where Monday=1, Tuesday=2, ..., Sunday=7'],
		examples is [
			'Get the Julian and the day-of-week # of a date:' - date(JD,2000,2,29,DoW) - {JD = 2451604, DoW = 2},
			'Check the validity of a given date (day-of-week is 2, not 4):' - date(JD,2002,3,5,4) - {no},
			'Get the Julian day of a given date if it is a Sunday:' - date(JD,2004,2,29,7) - {JD = 2453065},
			'Get the date and day-of-week # of a Julian:' - date(2451545,Y,M,D,DoW) - {Y = 2000, M = 1, D = 1, DoW = 6}]]).

	:- public(date/6).
	:- mode(date(?integer, ?integer, ?integer, ?integer, ?integer, ?compound), zero_or_one).
	:- info(date/6, [
		comment is 'Ditto date/5 + get/check its week #.',
		arguments is [
			'JD' - 'Julian day serial number',
			'Year' - '0 or negative if converted BC year, positive otherwise',
			'Month' - 'Normally an integer between 1 and 12 inclusive',
			'Day' - 'Normally an integer between 1 and 31 inclusive depending upon month',
			'DoW' - 'Day of week, where Monday=1, Tuesday=2, ..., Sunday=7',
			'Week' - 'Compound term, week(WeekNo,ActualYear), of a day'],
		examples is [
			'Get the day-of-week and week number of a date:' - date(_,2000,1,1,DoW,Wk) - {DoW = 6, Wk = week(52,1999)},
			'Get the week number and year of this week:' - date(_,_,_,_,_,Wk) - {Wk = week(7, 2004)},
			'Get the Julian number and the week of a date if it is a Sunday:' - date(JD,2004,2,29,7,Wk) - {JD = 2453065, Wk = week(9,2004)},
			'Get the day-of-week and week of a Julian day number:' - date(2453066,_,_,_,DoW,Wk) - {DoW = 1, Wk = week(10,2004)},
			'Check that given date data matches:' - date(_,2004,3,1,1,week(10,2004)) - {yes},
			'What is the date of a day of week (default is 1) in given week # and year?' - date(_,Y,M,D,DoW,week(26,2004)) - {Y = 2004, M = 6, D = 21, DoW = 1},
			'Ditto for Sunday:' - date(_,Y,M,D,7,week(1,2005)) - {Y = 2005, M = 1, D = 9},
			'Ditto for Tuesday in following week:' - date(_,Y,M,D,9,week(1,2005)) - {Y = 2005, M = 1, D = 11},
			'Ditto for Thursday in the prior week:' - date(_,Y,M,D,4,week(0,2005)) - {Y = 2004, M = 12, D = 30},
			'Ditto for Tuesday two weeks prior:' - date(_,Y,M,D,2,week(-1,2005)) - {Y = 2004, M = 12, D = 21},
			'Ditto for Saturday:' - date(_,Y,M,D,6,week(53,2004)) - {Y = 2005, M = 1, D = 1},
			'Ditto for Monday (note automatic compensation of nonexistent week number):' - date(_,Y,M,D,1,week(60,2004)) - {Y = 2005, M = 2, D = 14}]]).

	:- public(date/7).
	:- mode(date(?integer, ?integer, ?integer, ?integer, ?integer, ?compound, ?integer), zero_or_one).
	:- info(date/7, [
		comment is 'Ditto date/6 + get/check its day-of-year #.',
		arguments is [
			'JD' - 'Julian day serial number',
			'Year' - '0 or negative if converted BC year, positive otherwise',
			'Month' - 'Normally an integer between 1 and 12 inclusive',
			'Day' - 'Normally an integer between 1 and 31 inclusive depending upon month',
			'DoW' - 'Day of week, where Monday=1, Tuesday=2, ..., Sunday=7',
			'Week' - 'Compound term, week(WeekNo,ActualYear), of a day',
			'DoY' - 'Day of year (NB! calendar year, not week # year)'],
		examples is [
			'Get the date and day-of-year of a Julian number:' - date(2451649,Year,Month,Day,_,_,DoY) - {Year = 2000, Month = 4, Day = 14, DoY = 105},
			'Get the Julian number, week number and day-of-year of a date, confirming that it is a Sunday:' - date(JD,2004,2,29,7,Wk,DoY) - {JD = 2453065, Wk = week(9,2004), DoY = 60},
			'Confirm that a date is, in fact, a specific day-of-year:' - date(_,2004,3,1,_,_,61) - {yes},
			'Get the Julian number, week day and day-of-year of a date:' - date(JD,2004,10,18,DoW,_,DoY) - {JD = 2453297, DoW = 1, DoY = 292},
			'Get today''s day-of-year:' - date(_,_,_,_,_,_,DoY) - {DoY = 54},
			'Get all missing date data (excl. Julian number) for the 60th calendar day of 2004:' - date(_,2004,Month,Day,DoW,Week,60) - {Month = 2, Day = 29, DoW = 7, Week = week(9,2004)},
			'Match given date data and, if true, return the missing data (excl. Julian number):' - date(_,2004,3,Day,DoW,Week,61) - {Day = 1, DoW = 1, Week = week(10,2004)},
			'Ditto (the 61st day-of-year cannot be both day 1 and 2 of the month):' - date(_,2004,Month,2,DoW,Week,61) - {no}]]).

	:- public(date_string/3).
	:- mode(date_string(+atom, +integer, ?atom), zero_or_one).
	:- mode(date_string(+atom, ?list, ?atom), zero_or_one).
	:- info(date_string/3, [
		comment is 'Conversion between an ISO 8601 compliant date string and its components (truncated and expanded date representations are currently unsupported). Note that date components are not validated; that is the caller''s responsibility!',
		arguments is [
			'Format' - 'ISO 8601 format',
			'Components' - 'When bound and String is free, either a Julian number or a [Year,Month,Day] term; it binds to the system day/date if free When free and String is bound, it binds to an integer list representing the numeric elements of String',
			'String' - 'ISO 8601 formatted string correspondent to Components'],
		examples is [
			'Date, complete, basic (section 5.2.1.1):' - date_string('YYYYMMDD',[2004,2,29],Str) - {Str = '20040229'},
			'Date, complete, basic (section 5.2.1.1):' - date_string('YYYYMMDD',Day,'20040229') - {Day = [2004,2,29]},
			'Date, complete, extended (section 5.2.1.1):' - date_string('YYYY-MM-DD',[2003,12,16],Str) - {Str = '2003-12-16'},
			'Date, complete, extended (section 5.2.1.1):' - date_string('YYYY-MM-DD',Day,'2003-12-16') - {Day = [2003,12,16]},
			'Date, complete, extended (section 5.2.1.1):' - date_string('YYYY-MM-DD',_,Str) - {Str = '2004-02-17'},
			'Date, complete, extended (section 5.2.1.1):' - date_string('YYYY-MM-DD',Day,'2004-02-17') - {Day = [2004,2,17]},
			'Date, reduced, month (section 5.2.1.2 a):' - date_string('YYYY-MM',[2004,9,18],Str) - {Str = '2004-09'},
			'Date, reduced, month (section 5.2.1.2 a):' - date_string('YYYY-MM',Day,'2004-09') - {Day = [2004,9]},
			'Date, reduced, year (section 5.2.1.2 b):' - date_string('YYYY',[1900,7,24],Str) - {Str = '1900'},
			'Date, reduced, year (section 5.2.1.2 b):' - date_string('YYYY',Day,'1900') - {Day = [1900]},
			'Date, reduced, century (section 5.2.1.2 c):' - date_string('YY',2456557,Str) - {Str = '20'},
			'Date, reduced, century (section 5.2.1.2 c):' - date_string('YY',Day,'20') - {Day = [20]},
			'Date, ordinal, complete (section 5.2.2.1):' - date_string('YYYYDDD',[2005,3,25],Str) - {Str = '2005084'},
			'Date, ordinal, complete (section 5.2.2.1):' - date_string('YYYYDDD',Day,'2005084') - {Day = [2005,84]},
			'Date, ordinal, extended (section 5.2.2.1):' - date_string('YYYY-DDD',[1854,12,4],Str) - {Str = '1854-338'},
			'Date, ordinal, extended (section 5.2.2.1):' - date_string('YYYY-DDD',Day,'1854-338') - {Day = [1854,338]},
			'Week, complete, basic (section 5.2.3.1):' - date_string('YYYYWwwD',[2000,1,2],Str) - {Str = '1999W527'},
			'Week, complete, basic (section 5.2.3.1):' - date_string('YYYYWwwD',Day,'1999W527') - {Day = [1999,52,7]},
			'Week, complete, extended (section 5.2.3.1):' - date_string('YYYY-Www-D',[2003,12,29],Str) - {Str = '2004-W01-1'},
			'Week, complete, extended (section 5.2.3.1):' - date_string('YYYY-Www-D',Day,'2004-W01-1') - {Day = [2004,1,1]},
			'Week, complete, extended (section 5.2.3.1):' - date_string('YYYY-Www-D',2453167,Str) - {Str = '2004-W24-4'},
			'Week, complete, extended (section 5.2.3.1):' - date_string('YYYY-Www-D',Day,'2004-W24-4') - {Day = [2004,24,4]},
			'Week, reduced, basic (section 5.2.3.2):' - date_string('YYYYWww',[2004,2,29],Str) - {Str = '2004W09'},
			'Week, reduced, basic (section 5.2.3.2):' - date_string('YYYYWww',Day,'2004W09') - {Day = [2004,9]},
			'Week, reduced, extended (section 5.2.3.2):' - date_string('YYYY-Www',[2004,2,29],Str) - {Str = '2004-W09'},
			'Week, reduced, extended (section 5.2.3.2):' - date_string('YYYY-Www',Day,'2004-W09') - {Day = [2004,9]}]]).

	% MISCELLANEOUS PREDICATES (GOODIES):

	:- public(valid_date/3).
	:- mode(valid_date(+integer, +integer, +integer), zero_or_one).
	:- info(valid_date/3, [
		comment is 'Validate a given date in the Gregorian calendar.',
		argnames is ['Year', 'Month', 'Day'],
		examples is [
			'Yes, the recent millenium was a leap year:' - valid_date(2000,2,29) - {yes},
			'2004 was also a leap year:' - valid_date(2004,2,29) - {yes},
			'Only 30 days in April:' - valid_date(2004,4,31) - {no},
			'1 BC was a leap year:' - valid_date(-1,2,29) - {yes}
	  ]]).

	:- public(leap_year/1).
	:- mode(leap_year(?integer), zero_or_one).
	:- info(leap_year/1, [
		comment is 'Succeed if given year is a leap year in the Gregorian calendar.',
		arguments is [
			'Year' - 'The Gregorian calendar year to investigate. If free, it binds to the system year'],
		examples is [
			'No, the prior centenary was not a leap year:' - leap_year(1900) - {no},
			'The recent millenium:' - leap_year(2000) - {yes},
			'This year:' - leap_year(Year) - {Year = 2004},
			'This year (equivalent to prior query):' - leap_year(_) - {yes},
			'Next centennial:' - leap_year(2100) - {no},
			'Year 0, equivalent to 1 BC:' - leap_year(0) - {yes},
			'1 BC' - leap_year(-1) - {yes},
			'4 BC' - leap_year(-4) - {no},
			'5 BC' - leap_year(-5) - {yes}
		]]).

	:- public(calendar_month/3).
	:- mode(calendar_month(?integer, ?integer, -compound), zero_or_one).
	:- info(calendar_month/3, [
		comment is 'Compute a calendar month.',
		arguments is [
			'Year' - 'The calendar year',
			'Month' - 'The calendar month',
			'Calendar' - 'A compound term, m/3, composed of three main arguments specifying year, month, and a list of week and week day numbers (calendar body).'],
		examples is [
			'Compute the calendar of March, 2005:' - calendar_month(2005, 3, Calendar) - {Calendar = m(2005, 3,[w( 9, [ 0,  1,  2,  3,  4,  5,  6]),w(10, [ 7,  8,  9, 10, 11, 12, 13]),w(11, [14, 15, 16, 17, 18, 19, 20]),w(12, [21, 22, 23, 24, 25, 26, 27]),w(13, [28, 29, 30, 31,  0,  0, 0]),w( 0, [ 0,  0,  0,  0,  0,  0,  0])])}]]).

	:- public(easter_day/3).
	:- mode(easter_day(?integer, -integer, -integer), zero_or_one).
	:- info(easter_day/3, [
		comment is 'Compute a Gregorian Easter Sunday.',
		arguments is [
			'Year' - 'Integer specifying the year to be investigated',
			'Month' - 'Month in which Easter Sunday falls for given year',
			'Day'- 'Day of month in which Easter Sunday falls for given year'],
		examples is [
			'Compute Easter Sunday for a particular year:' - easter_day(2006,Month,Day) - {Month=4, Day=16},
			'Compute Easter Sunday for the current year:' - easter_day(Year,Month,Day) - {Year = 2005, Month = 3, Day = 27}
			]]).


	/************************
	 ISO 8601 DATE PREDICATES
	 ************************/

	%==============================================================================
	% date(?JD, ?Year, ?Month, ?Day)

	date(JD,Year,Month,Day) :-
		(  var(JD), var(Year),  var(Month),  var(Day)
			-> % GET THE SYSTEM DATE AND ITS JULIAN DAY SERIAL NUMBER:
			{'$lgt_current_date'(Year, Month, Day)}
			;  true
		),
		(  var(JD), nonvar(Year),  nonvar(Month),  nonvar(Day)
			-> % CORRIGATE BC/AD CALENDAR YEARS TO FIT 0-BASED ALGORITHM:
			(Year < 0 -> Year1 is Year + 1 ; Year1 = Year),
			% CONVERT DATE PARTS TO JULIAN DAY SERIAL NUMBER:
			A is (14 - Month) // 12,
			Y is Year1 + 4800 - A,
			M is Month + (12 * A) - 3,
			D is Day + ((153 * M + 2) // 5) + (365 * Y) + (Y // 4),
			JD is D - (Y // 100) + (Y // 400) - 32045
		;  nonvar(JD),
			% CONVERT JULIAN DAY SERIAL NUMBER TO DATE PARTS:
			A is JD + 32045,
			B is (4 * (A + 36524)) // 146097 - 1,
			C is A - ((146097 * B) // 4),
			D is ((4 * (C + 365)) // 1461) - 1,
			E is C - ((1461 * D) // 4),
			M is ((5 * (E - 1)) + 2) // 153,
			Day is E - (((153 * M) + 2) // 5),
			Month is M + (3 - (12 * (M // 10))),
			Year1 is ((100 * B) + D - 4800 + (M // 10)),
			% CORRIGATE 0-BASED ALGORITHM RESULT TO BC/AD CALENDAR YEARS:
			(Year1 < 1 -> Year is Year1 - 1 ;  Year = Year1)
		).


	%==============================================================================
	% date(?JD, ?Year, ?Month, ?Day, ?DoW)

	date(JD,Year,Month,Day,DoW) :-
		date(JD,Year,Month,Day),
		DoW is (JD mod 7) + 1.


	%==============================================================================
	% date(?JD, ?Year, ?Month, ?Day, ?DoW, ?Week)

	date(JD,Year,Month,Day,DoW,week(Wk,Yr)) :-
		(  var(JD), var(Year), var(Month), var(Day), nonvar(Wk), nonvar(Yr)
			-> (var(DoW) -> DoW = 1 ; true),
			date(JD1,Yr,1,1,DoW1),
			(DoW1 > 4 -> Offset = 0 ; Offset = 1),
			JD is JD1 + ((Wk - Offset) * 7) + DoW - DoW1,
			date(JD,Year,Month,Day)
		;  date(JD,Year,Month,Day,DoW),
			D4 is (((JD + 31741 - (JD mod 7)) mod 146097) mod 36524) mod 1461,
			L is D4 // 1460,
			Wk is (((D4 - L) mod 365) + L) // 7 + 1,
			% CORRIGATE YEAR AS NECESSARY:
			(  Month =:= 1, (Day =:= 1 ; Day =:= 2 ; Day =:= 3), Wk > 1
				-> Yr is Year - 1
			;  (  Month =:= 12, (Day =:= 29 ; Day =:= 30 ; Day =:= 31), Wk =:= 1
					-> Yr is Year + 1
				;  Yr = Year
				)
			)
		).


	%==============================================================================
	% date(?JD, ?Year, ?Month, ?Day, ?DoW, ?Week, ?DoY)

	date(JD,Year,Month,Day,DoW,Week,DoY) :-
		(	var(JD), nonvar(Year), (var(Month) ; var(Day)), nonvar(DoY)
			-> date(JD1,Year,1,0),
			JD is JD1 + DoY,
			date(JD,Year,Month,Day,DoW,Week)
		;	date(JD,Year,Month,Day,DoW,Week),
			date(JD1,Year,1,0),
			DoY is JD - JD1
		).


	%==============================================================================
	% date_string(+Format, ?Day, ?String)

	date_string('YYYYMMDD',Day,String) :-	% DATE
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,M0,M1,D0,D1]),
			  number_chars(Y,[Y0,Y1,Y2,Y3]),
			  number_chars(M,[M0,M1]),
			  number_chars(D,[D0,D1]),
			  Day = [Y,M,D]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			  (Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; date(Day,Y,M,D)),
			  prepend_zeros(2,D,D1),
			  prepend_zeros(2,M,M1),
			  number_codes(Y,Y1),
			  list_of_lists_to_atom([Y1,M1,D1],String)
		).
	date_string('YYYY-MM-DD',Day,String) :-  % DATE
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,M0,M1,_,D0,D1]),
			  number_chars(Y,[Y0,Y1,Y2,Y3]),
			  number_chars(M,[M0,M1]),
			  number_chars(D,[D0,D1]),
			  Day = [Y,M,D]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			  (Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; date(Day,Y,M,D)),
			  prepend_zeros(2,D,D1),
			  prepend_zeros(2,M,M1),
			  number_codes(Y,Y1),
			  list_of_lists_to_atom([Y1,[45],M1,[45],D1],String)
		).
	date_string('YYYY-MM',Day,String) :-	  % YEAR & MONTH
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,M0,M1]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(Month,[M0,M1]),
			  Day = [Year,Month]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,_) ; true),
			  (Day = [Y,M,_] -> nonvar(Y), nonvar(M) ; date(Day,Y,M,_)),
			  prepend_zeros(2,M,M1),
			  number_codes(Y,Y1),
			  list_of_lists_to_atom([Y1,[45],M1],String)
		).
	date_string('YYYY',Day,String) :-		  % YEAR
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  Day = [Year]
		;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,_,_) ; true),
			(Day = [Y|_] -> nonvar(Y) ; date(Day,Y,_,_)),
			number_codes(Y,Codes),
			atom_codes(String,Codes)
		).
	date_string('YY',Day,String) :-			% CENTURY
		( nonvar(String)
		  -> atom_chars(String,[C0,C1]),
			  number_chars(Century,[C0,C1]),
			  Day = [Century]
		;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,_,_) ; true),
			(Day = [Y|_] -> nonvar(Y) ; date(Day,Y,_,_)),
			Y1 is Y // 100,
			number_codes(Y1,Codes),
			atom_codes(String,Codes)
		).
	date_string('YYYYDDD',Day,String) :-	  % YEAR & DAY-OF-YEAR
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,D0,D1,D2]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(DoY,[D0,D1,D2]),
			  Day = [Year,DoY]
		;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			(Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; JD = Day),
			date(JD,Y,M,D,_,_,DoY),
			prepend_zeros(3,DoY,DoY1),
			number_codes(Y,Y1),
			list_of_lists_to_atom([Y1,DoY1],String)
		).
	date_string('YYYY-DDD',Day,String) :-	% YEAR & DAY-OF-YEAR
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,D0,D1,D2]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(DoY,[D0,D1,D2]),
			  Day = [Year,DoY]
		;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			(Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; JD = Day),
			date(JD,Y,M,D,_,_,DoY),
			prepend_zeros(3,DoY,DoY1),
			number_codes(Y,Y1),
			list_of_lists_to_atom([Y1,[45],DoY1],String)
		).
	date_string('YYYYWwwD',Day,String) :-	% YEAR, WEEK & DAY-OF-WEEK
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,W0,W1,DoW0]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(Week,[W0,W1]),
			  number_chars(DoW,[DoW0]),
			  Day = [Year,Week,DoW]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			  (Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; JD = Day),
			  date(JD,Y,M,D,DoW,week(Wk,Yr)),
			  number_codes(Yr,Y1),
			  prepend_zeros(2,Wk,Wk1),
			  number_codes(DoW,DoW1),
			  List = [Y1,[87],Wk1,DoW1],
			  list_of_lists_to_atom(List,String)
		).
	date_string('YYYY-Www-D',Day,String) :-  % YEAR, WEEK & DAY-OF-WEEK
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,_,W0,W1,_,DoW0]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(Week,[W0,W1]),
			  number_chars(DoW,[DoW0]),
			  Day = [Year,Week,DoW]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			  (Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; JD = Day),
			  date(JD,Y,M,D,DoW,week(Wk,Yr)),
			  number_codes(Yr,Y1),
			  prepend_zeros(2,Wk,Wk1),
			  number_codes(DoW,DoW1),
			  List = [Y1,[45,87],Wk1,[45],DoW1],
			  list_of_lists_to_atom(List,String)
		).
	date_string('YYYYWww',Day,String) :-	  % YEAR & WEEK
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,W0,W1]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(Week,[W0,W1]),
			  Day = [Year,Week]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			  (Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; JD = Day),
			  date(JD,Y,M,D,_,week(Wk,Yr)),
			  number_codes(Yr,Y1),
			  prepend_zeros(2,Wk,Wk1),
			  List = [Y1,[87],Wk1],
			  list_of_lists_to_atom(List,String)
		).
	date_string('YYYY-Www',Day,String) :-	% YEAR & WEEK
		( nonvar(String)
		  -> atom_chars(String,[Y0,Y1,Y2,Y3,_,_,W0,W1]),
			  number_chars(Year,[Y0,Y1,Y2,Y3]),
			  number_chars(Week,[W0,W1]),
			  Day = [Year,Week]
		  ;  ((var(Day) ; Day = [Y|_], var(Y)) -> date(_,Y,M,D) ; true),
			  (Day = [Y,M,D] -> nonvar(Y), nonvar(M), nonvar(D) ; JD = Day),
			  date(JD,Y,M,D,_,week(Wk,Yr)),
			  number_codes(Yr,Y1),
			  prepend_zeros(2,Wk,Wk1),
			  List = [Y1,[45,87],Wk1],
			  list_of_lists_to_atom(List,String)
		).


	%-----------------------------------
	% prepend_zeros(+Digits, +N, -Codes)
	% Purpose: prepend zeros to a given integer
	% Parameters:
	%  Digits: required number of digits to be entered into Codes
	%  N:  integer to which zeros are prepended
	%  Codes:  the resulting list of codes
	% Called by: date_string/3
	% Examples:
	%  ?- prepend_zeros(2,2,Codes).  => Codes = [48,50]
	%  ?- prepend_zeros(2,22,Codes). => Codes = [50,50]
	%  ?- prepend_zeros(3,2,Codes).  => Codes = [48,48,50]
	%  ?- prepend_zeros(3,22,Codes). => Codes = [48,50,50]

	prepend_zeros(2, I, Codes) :-
		number_codes(I, ICodes),
		two_codes(ICodes, Codes).
	prepend_zeros(3, I, Codes) :-
		number_codes(I, ICodes),
		three_codes(ICodes, Codes).

	two_codes([A], [48, A]) :- !.
	two_codes([A, B], [A, B]).

	three_codes([A], [48, 48, A]) :- !.
	three_codes([A, B], [48, A, B]) :- !.
	three_codes([A, B, C], [A, B, C]).

	%---------------------------------------
	% list_of_lists_to_atom(+Llist, -String)
	% Purpose: Convert a list of code lists to a string
	% Called by: date_string/3

	list_of_lists_to_atom(Llist,String) :-
		flatten(Llist,Codes),
		atom_codes(String,Codes).

	%------------------------------
	% flatten(+Llist, -Codes)
	% Purpose: Convert a list of lists to a list of codes
	% Note: custom, simplified version
	% Called by: list_of_lists_to_atom/2

	flatten([], []).
	flatten([[]| Ls], F) :-
		!,
		flatten(Ls, F).
	flatten([[H| T]| Ls], [H| Fs]) :-
		flatten([T| Ls], Fs).



	/**********************************
	 MISCELLANEOUS PREDICATES (GOODIES)
	 **********************************/

	%==============================================================================
	% valid_date(+Year, +Month, +Day)

	valid_date(Year,Month,Day) :-
		Month > 0,  Month < 13,
		Day > 0,
		(  Month =:= 2
			-> (leap_year(Year) -> Days = 29 ;  Days = 28)
		;  days_in_month(Month,Days)
		),
		Day =< Days.


	%==============================================================================
	% leap_year(+Year)

	leap_year(Year) :-
		(var(Year) -> date(_,Year,_,_) ; true),
		(Year < 0 -> Year1 is Year + 1 ; Year1 = Year),
		0 =:= Year1 mod 4,
		(\+ 0 =:= Year1 mod 100 -> true ;  0 =:= Year1 mod 400).


	%==============================================================================
	% calendar_month(?Year, ?Month, -Calendar)

	calendar_month(Year,Month,m(Year,Month,Weeks)) :-
		(var(Year), var(Month) -> date(_,Year,Month,_) ; true),
		% COMPUTE THE BODY (A 6 ROW BY 8 COLUMN TABLE OF WEEK AND DAY NUMBERS):
		date(JD,Year,Month,1,DoW,week(Week,_)),
		Lead0s is DoW - 1,  % number of leading zeros required
		( Month =:= 2
		  -> (leap_year(Year) -> Days = 29 ;  Days = 28)
		  ;  days_in_month(Month,Days)
		),
		Delta is 42 - (Lead0s + Days),	% number of trailing zeros required
		zeros(Delta,[],Append),		 % zeros to be appended to day list
		day_list(Days,Append,DList),  % create padded daylist
		zeros(Lead0s,DList,DayList),  % prepend zeros to padded day list
		Julian is JD - Lead0s,
		week_list(6,Julian,Week,DayList,Weeks).

	%-------------------------------
	% zeros(+Counter, +Build, -List)
	% Purpose: Prepend or append a list of 0's (zeros) to a given list
	% Called by: calendar_month/3

	zeros(0,L,L):- !.
	zeros(DoW,Build,List) :-
		Next is DoW - 1,
		zeros(Next,[0|Build],List).

	%-------------------------------------
	% day_list(+Counter, +Build, -DayList)
	% Purpose: Return a list of day #s
	% Called by: calendar_month/3

	day_list(0,DayList,DayList) :- !.
	day_list(N,SoFar,DayList) :-
		N1 is N - 1,
		day_list(N1,[N|SoFar],DayList).

	%-------------------------------------------
	% week_list(+N, +JD, +Week, +Days, -WeekList)
	% Purpose: Return a list of week and day #s
	% Called by: calendar_month/3

	week_list(0,_,_,_,[]).
	week_list(N,JD,Week,Days,[X|Weeks]) :-
		Days = [F1,F2,F3,F4,F5,F6,F7|Days1],
		(  N < 3,
			F1 =:= 0
			-> Wk = 0
		;  Wk = Week
		),
		X = w(Wk,[F1,F2,F3,F4,F5,F6,F7]),
		JD1 is JD + 7,
		(  Week > 51
			-> date(JD1,_,_,_,_,week(Week1,_))
		;  Week1 is Week + 1
		),
		N1 is N - 1,
		week_list(N1,JD1,Week1,Days1,Weeks).


	%==============================================================================
	% easter_day(?Year, -Month, -Day)

	easter_day(Year,Month,Day):-
		(nonvar(Year) -> true ; date(_,Year,_,_)),
		Year > 1582,
		A is Year mod 19,
		B is Year mod 4,
		C is Year mod 7,
		calc_M_and_N(Year,M,N),
		D is (19 * A + M) mod 30,
		E is ((2 * B) + (4 * C) + (6 * D) + N) mod 7,
		R is 22 + D + E,
		calc_month_and_day(R,Month,Day1),
		corr_day(Day1,Month,A,D,E,Day).

	%----------------------------
	% calc_M_and_N(+Year, -M, -N)
	% Purpose: Calculate intermediate values M and N
	% Called by: easter_day/3

	calc_M_and_N(Year,M,N):-
		T is Year // 100,
		P is (13 + 8 * T) // 25,
		Q is T // 4,
		M is (15 + T - P - Q) mod 30,
		N is (T - (T // 4) + 4) mod 7.

	%-------------------------------------
	% calc_month_and_day(+R, -Month, -Day)
	% Purpose: Calculate the Easter Sunday month and likely day
	% Called by: easter_day/3

	calc_month_and_day(R,4,Day):-  % April
		R > 31,
		!,
		Day is R - 31.
	calc_month_and_day(R,3,R).	% March

	%---------------------------------------------------
	% corr_day(+PossDay, +Month, +A, +D, +E, -ActualDay)
	% Purpose: Calculate the actual Easter Sunday
	% Called by: easter_day/3

	corr_day(_,4,_,29,6,19):-  % April, Gregorian exception 1
		!.
	corr_day(_,4,A,28,6,18):-  % April, Gregorian exception 2
		A > 10,
		!.
	corr_day(Day,_,_,_,_,Day).  % Otherwise



	/************************
	 LOCAL UTILITY PREDICATES
	 ************************/

	%------------------------------------
	% days_in_month(+Month, -DaysInMonth)
	% Purpose: Return the number of days in a given month
	% Called by: valid_date/3, calendar_month/3

	days_in_month( 1, 31).
	days_in_month( 2, 28).
	days_in_month( 3, 31).
	days_in_month( 4, 30).
	days_in_month( 5, 31).
	days_in_month( 6, 30).
	days_in_month( 7, 31).
	days_in_month( 8, 31).
	days_in_month( 9, 30).
	days_in_month(10, 31).
	days_in_month(11, 30).
	days_in_month(12, 31).


:- end_object.
