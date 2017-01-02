%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(profilerp).

	:- info([
		version is 1.02,
		author is 'Paulo Moura',
		date is 2015/02/13,
		comment is 'Simple wrapper protocol for Prolog native profilers.'
	]).

	:- public(load/1).
	:- mode(load(@file), zero_or_one).
	:- info(load/1, [
		comment is 'Compiles and loads a Logtalk source file for profiling.',
		argnames is ['File']
	]).

	:- public(load/2).
	:- mode(load(@file, @list), zero_or_one).
	:- info(load/2, [
		comment is 'Compiles and loads a Logtalk source file for profiling using a set of flags.',
		argnames is ['File', 'Flags']
	]).

	:- public(profile/1).
	:- meta_predicate(profile(0)).
	:- mode(profile(@callable), zero_or_more).
	:- info(profile/1, [
		comment is 'Proves a goal while collecting profiling information.',
		argnames is ['Goal']
	]).

	:- public(data/0).
	:- mode(data, one).
	:- info(data/0, [
		comment is 'Prints a table with all profiling data.'
	]).

	:- public(data/1).
	:- mode(data(@entity_identifier), one).
	:- info(data/1, [
		comment is 'Prints a table with all profiling data for a given entity.',
		argnames is ['Entity']
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all profiling data.'
	]).

:- end_protocol.
