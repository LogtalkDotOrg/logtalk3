%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(debuggerp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/04/23,
		comment is 'Debugger protocol.'
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all debugging settings, including spy points and context spy points.'
	]).

	:- public(debug/0).
	:- mode(debug, one).
	:- info(debug/0, [
		comment is 'Starts debugging for all defined spy points and context spy points.'
	]).

	:- public(nodebug/0).
	:- mode(nodebug, one).
	:- info(nodebug/0, [
		comment is 'Stops debugging for all defined spy points and context spy points.'
	]).

	:- public(debugging/0).
	:- mode(debugging, one).
	:- info(debugging/0, [
		comment is 'Reports current debugging settings, including spy points and context spy points.'
	]).

	:- public(debugging/1).
	:- mode(debugging(@dictionary), zero_or_one).
	:- info(debugging/1, [
		comment is 'True if the entity is being debugged.',
		argnames is ['Entity']
	]).

	:- public(trace/0).
	:- mode(trace, one).
	:- info(trace/0, [
		comment is 'Starts tracing all calls compiled in debug mode.'
	]).

	:- public(notrace/0).
	:- mode(notrace, one).
	:- info(notrace/0, [
		comment is 'Stops tracing of call compiled in debug mode.'
	]).

	:- public((spy)/1).
	:- mode(spy(+predicate_indicator), one).
	:- mode(spy(+list(predicate_indicator)), one).
	:- info((spy)/1, [
		comment is 'Sets a spy point for a predicate or a list of predicates.',
		argnames is ['Predicate']
	]).

	:- public((spy)/4).
	:- mode(spy(@term, @term, @term, @term), one).
	:- info((spy)/4, [
		comment is 'Sets a context spy point.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public((nospy)/1).
	:- mode(nospy(@variable), one).
	:- mode(nospy(+predicate_indicator), one).
	:- mode(nospy(+list(predicate_indicator)), one).
	:- info((nospy)/1, [
		comment is 'Removes all matching predicate spy points.',
		argnames is ['Predicate']
	]).

	:- public((nospy)/4).
	:- mode(nospy(@callable, +dictionary, -dictionary, ?), one).
	:- info((nospy)/4, [
		comment is 'Removes all matching context spy points.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public(nospyall/0).
	:- mode(nospyall, one).
	:- info(nospyall/0, [
		comment is 'Removes all spy points.'
	]).

	:- public(leash/1).
	:- mode(leash(+atom), one).
	:- mode(leash(+list(atom)), one).
	:- info(leash/1, [
		comment is 'Sets the debugger leash ports using an abbreviation (none, loose, half, tight, or full) or a list of ports (valid ports are fact, rule, call, exit, redo, fail, and exception).',
		argnames is ['Ports']
	]).

:- end_protocol.
