%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(randomp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/10/31,
		comment is 'Random number generator protocol.']).

	:- public(random/1).
	:- synchronized(random/1).
	:- mode(random(-float), one).
	:- info(random/1, [
		comment is 'Returns a new random float value in the interval [0.0, 1.0[.',
		argnames is ['Random']]).

	:- public(random/3).
	:- synchronized(random/3).
	:- mode(random(+integer, +integer, -integer), zero_or_one).
	:- mode(random(+float, +float, -float), zero_or_one).
	:- info(random/3, [
		comment is 'Returns a new random value in the interval [Lower, Upper[.',
		argnames is ['Lower', 'Upper', 'Random']]).

	:- public(randseq/4).
	:- synchronized(randseq/4).
	:- mode(randseq(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randseq(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randseq/4, [
		comment is 'Returns a list of Length random values in the interval [Lower, Upper[.',
		argnames is ['Length', 'Lower', 'Upper', 'List']]).

	:- public(randset/4).
	:- synchronized(randset/4).
	:- mode(randset(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randset(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randset/4, [
		comment is 'Returns an ordered set of Length random values in the interval [Lower, Upper[.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']]).

	:- public(reset_seed/0).
	:- synchronized(reset_seed/0).
	:- mode(reset_seed, one).
	:- info(reset_seed/0, [
		comment is 'Resets the random seed to its default value.']).

	:- public(set_seed/1).
	:- synchronized(set_seed/1).
	:- mode(set_seed(+integer), zero_or_one).
	:- info(set_seed/1, [
		comment is 'Sets the random seed to the given value.',
		argnames is ['Seed']]).

:- end_protocol.
