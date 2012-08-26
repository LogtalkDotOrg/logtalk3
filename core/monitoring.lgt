%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(monitoring).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/04/15,
		comment is 'Event handlers protocol.']).

	:- public(before/3).
	:- mode(before(@term, @term, @term), zero_or_one).
	:- info(before/3, [
		comment is 'Event handler for "before" events.',
		argnames is ['Object', 'Message', 'Sender']]).

	:- public(after/3).
	:- mode(after(@term, @term, @term), zero_or_one).
	:- info(after/3, [
		comment is 'Event handler for "after" events.',
		argnames is ['Object', 'Message', 'Sender']]).

:- end_protocol.
