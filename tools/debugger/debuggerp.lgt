%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(debuggerp).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2016/10/14,
		comment is 'Debugger protocol.',
		remarks is [
			'Debugger help' - 'Type the character "h" or the character "?" at a leashed port.',
			'Predicate spy point' - 'Specified as a ground term Functor/Arity.',
			'Line number spy point' - 'Specified as an Entity-Line term with both Entity and Line bound.',
			'Leash shorthands' - 'none - [], loose - [fact, rule, call], half - [fact, rule, call, redo], tight - [fact, rule, call, redo, fail, exception], and full - [fact, rule, call, exit, redo, fail, exception].'
		]
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all debugging settings, including spy points and leashed ports, and turns off debugging.'
	]).

	:- public(debug/0).
	:- mode(debug, one).
	:- info(debug/0, [
		comment is 'Starts debugging for all defined spy points.'
	]).

	:- public(nodebug/0).
	:- mode(nodebug, one).
	:- info(nodebug/0, [
		comment is 'Stops debugging for all defined spy points.'
	]).

	:- public(debugging/0).
	:- mode(debugging, one).
	:- info(debugging/0, [
		comment is 'Reports current debugging settings, including spy points.'
	]).

	:- public(debugging/1).
	:- mode(debugging(?entity_identifier), zero_or_more).
	:- info(debugging/1, [
		comment is 'Enumerates, by backtracking, all entities compiled in debug mode.',
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
		comment is 'Stops tracing of calls compiled in debug mode.'
	]).

	:- public((spy)/1).
	:- mode(spy(@spy_point), one).
	:- mode(spy(@list(spy_point)), one).
	:- info((spy)/1, [
		comment is 'Sets a line number spy point or a predicate spy point or a list of spy points.',
		argnames is ['SpyPoint']
	]).

	:- public(spying/1).
	:- mode(spying(?spy_point), zero_or_more).
	:- info(spying/1, [
		comment is 'Enumerates, by backtracking, all defined line number and predicate spy points.',
		argnames is ['SpyPoint']
	]).

	:- public((spy)/4).
	:- mode(spy(@term, @term, @term, @term), one).
	:- info((spy)/4, [
		comment is 'Sets a context spy point.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public(spying/4).
	:- mode(spying(?term, ?term, ?term, ?term), zero_or_more).
	:- info(spying/4, [
		comment is 'Enumerates, by backtracking, all defined context spy points.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public((nospy)/1).
	:- mode(nospy(@var), one).
	:- mode(nospy(@spy_point), one).
	:- mode(nospy(@list(spy_point)), one).
	:- info((nospy)/1, [
		comment is 'Removes all matching line number spy points and predicate spy points.',
		argnames is ['SpyPoint']
	]).

	:- public((nospy)/4).
	:- mode(nospy(@term, @term, @term, @term), one).
	:- info((nospy)/4, [
		comment is 'Removes all matching context spy points.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public(nospyall/0).
	:- mode(nospyall, one).
	:- info(nospyall/0, [
		comment is 'Removes all line number, predicate, and context spy points.'
	]).

	:- public(leash/1).
	:- mode(leash(+atom), one).
	:- mode(leash(+list(atom)), one).
	:- info(leash/1, [
		comment is 'Sets the debugger leash ports using an abbreviation (none, loose, half, tight, or full) or a list of ports (valid ports are fact, rule, call, exit, redo, fail, and exception).',
		argnames is ['Ports']
	]).

	:- public(leashing/1).
	:- mode(leashing(?atom), zero_or_more).
	:- info(leashing/1, [
		comment is 'Enumerates, by backtracking, all leashed ports (valid ports are fact, rule, call, exit, redo, fail, and exception).',
		argnames is ['Port']
	]).

:- end_protocol.
