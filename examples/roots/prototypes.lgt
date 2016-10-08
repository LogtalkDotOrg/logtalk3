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


:- protocol(protop).

	:- info([
		version is 1.0,
		date is 2000/7/24,
		author is 'Paulo Moura',
		comment is 'Default protocol for all prototypes.'
	]).

	:- public(clone/1).
	:- mode(clone(?object), zero_or_one).
	:- info(clone/1, [
		comment is 'Clones a prototype.',
		argnames is ['Clone']
	]).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0, [
		comment is 'Pretty prints an object description.'
	]).

:- end_protocol.



:- object(proto,
	implements((protop, monitoring)),
	imports(((initialization), proto_hierarchy))).

	:- info([
		version is 1.2,
		date is 2006/12/14,
		author is 'Paulo Moura',
		comment is 'Minimal predicates for all prototypes. Default root of the extension graph.'
	]).

	:- uses(event_registry, [del_monitors/4]).

	clone(Clone) :-
		self(Self),
		sender(Sender),
		throw(error(descendant_responsability, Self::clone(Clone), Sender)).

	default_free_option(del_monitors).

	process_free_option(del_monitors) :-
		self(Self),
		del_monitors(Self, _, _, _),
		del_monitors(_, _, Self, _),
		del_monitors(_, _, _, Self).

	print :-
		self(Self),
		writeq(Self), nl, nl,
		forall(
			::current_predicate(Predicate),
			(writeq(Predicate), nl)),
		nl.

	before(_, _, _).

	after(_, _, _).

:- end_object.
