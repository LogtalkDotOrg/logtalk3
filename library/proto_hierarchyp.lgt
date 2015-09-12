%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(proto_hierarchyp,
	extends(hierarchyp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/2/20,
		comment is 'Prototype hierarchy protocol.'
	]).

	:- public(parent/1).
	:- mode(parent(?object), zero_or_more).
	:- info(parent/1, [
		comment is 'Returns, by backtracking, all object parents.',
		argnames is ['Parent']
	]).

	:- public(parents/1).
	:- mode(parents(-list), one).
	:- info(parents/1, [
		comment is 'List of all object parents.',
		argnames is ['Parents']
	]).

	:- public(extension/1).
	:- mode(extension(?object), zero_or_more).
	:- info(extension/1, [
		comment is 'Returns, by backtracking, all object direct descendants.',
		argnames is ['Extension']
	]).

	:- public(extensions/1).
	:- mode(extensions(-list), one).
	:- info(extensions/1, [
		comment is 'List of all object direct descendants.',
		argnames is ['Extensions']
	]).

:- end_protocol.
