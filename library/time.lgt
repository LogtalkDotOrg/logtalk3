%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(time,
	implements(timep)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2014/9/27,
		comment is 'Time predicates.'
	]).

	now(Hours, Mins, Secs) :-
		os::date_time(_, _, _, Hours, Mins, Secs, _).

	cpu_time(Seconds) :-
		os::cpu_time(Seconds).

	valid(Hours, Mins, Secs) :-
		integer(Hours), Hours >= 0,
		integer(Mins), Mins >= 0, Mins =< 59,
		integer(Secs), Secs >= 0, Secs =< 59.

:- end_object.
