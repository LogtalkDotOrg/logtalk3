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



:- protocol(hierarchyp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Common hierarchy protocol for prototype and class hierarchies.'
	]).

	:- public(ancestor/1).
	:- mode(ancestor(?object), zero_or_more).
	:- info(ancestor/1, [
		comment is 'Returns, by backtracking, all object ancestors.',
		argnames is ['Ancestor']
	]).

	:- public(ancestors/1).
	:- mode(ancestors(-list), one).
	:- info(ancestors/1, [
		comment is 'List of all object ancestors.',
		argnames is ['Ancestors']
	]).

	:- public(leaf/1).
	:- mode(leaf(?object), zero_or_more).
	:- info(leaf/1, [
		comment is 'Returns, by backtracking, all object leaves.',
		argnames is ['Leaf']
	]).

	:- public(leaves/1).
	:- mode(leaves(-list), one).
	:- info(leaves/1, [
		comment is 'List of all object leaves.',
		argnames is ['Leaves']
	]).

	:- public(descendant/1).
	:- mode(descendant(?object), zero_or_more).
	:- info(descendant/1, [
		comment is 'Returns, by backtracking, all object descendants.',
		argnames is ['Descendant']
	]).

	:- public(descendants/1).
	:- mode(descendants(-list), one).
	:- info(descendants/1, [
		comment is 'List of all object descendants.',
		argnames is ['Descendants']
	]).

:- end_protocol.
