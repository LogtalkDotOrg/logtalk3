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



:- protocol(comparingp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Comparing protocol using overloading of standard operators.'
	]).

	:- public((<)/2).
	:- mode(<(+term, +term), zero_or_one).
	:- info((<)/2, [
		comment is 'True if Term1 is less than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((=<)/2).
	:- mode(=<(+term, +term), zero_or_one).
	:- info((=<)/2, [
		comment is 'True if Term1 is less or equal than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((>)/2).
	:- mode(>(+term, +term), zero_or_one).
	:- info((>)/2, [
		comment is 'True if Term1 is greater than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((>=)/2).
	:- mode(>=(+term, +term), zero_or_one).
	:- info((>=)/2, [
		comment is 'True if Term1 is equal or grater than Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((=:=)/2).
	:- mode(=:=(+term, +term), zero_or_one).
	:- info((=:=)/2, [
		comment is 'True if Term1 is equal to Term2.',
		argnames is ['Term1', 'Term2']
	]).

	:- public((=\=)/2).
	:- mode(=\=(+term, +term), zero_or_one).
	:- info((=\=)/2, [
		comment is 'True if Term1 is not equal to Term2.',
		argnames is ['Term1', 'Term2']
	]).

:- end_protocol.
