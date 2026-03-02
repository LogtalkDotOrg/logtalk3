%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(list_permutations).

	:- info([
		version is 1:1:0,
		author is 'Paul Tarau; adapted to Logtalk by Paulo Moura',
		date is 2026-03-02,
		comment is 'List permutation predicates for benchmarking.'
	]).

	:- public(backtracking/1).
	:- mode(backtracking(+integer), one).
	:- info(backtracking/1, [
		comment is 'Generates all permutations of a list of natural numbers of the given length by backtracking.',
		argnames is ['Length']
	]).

	:- public(list/2).
	:- mode(list(+integer, -list), one).
	:- info(list/2, [
		comment is 'Computes and returns a list of all permutations of a list of natural numbers of the given length.',
		argnames is ['Length', 'Permutations']
	]).

	:- public(all/2).
	:- mode(all(+integer, -list), one).
	:- info(all/2, [
		comment is 'Uses the findall/3 predicate to compute and return a list of all permutations of a list of natural numbers of the given length.',
		argnames is ['Length', 'Permutations']
	]).

	:- public(map/2).
	:- mode(map(+integer, -list), one).
	:- info(map/2, [
		comment is 'Computes a list of all permutations of a list of natural numbers of the given length and maps the copy_term/2 predicate over it returning the new list.',
		argnames is ['Length', 'Permutations']
	]).

	:- public(copy/1).
	:- mode(copy(+integer), one).
	:- info(copy/1, [
		comment is 'Computes a list of all permutations of a list of natural numbers of the given length and calls the copy_term/2 predicate over it via backtracking.',
		argnames is ['Length']
	]).

	allperms([],[[]]).
	allperms([X|Xs],Perms2) :-
		allperms(Xs,Perms1),
		extendperms(Perms1,X,Perms2).

	extendperms([],_,[]).
	extendperms([Perm|Perms1],X,[[X|Perm]|Perms3]) :-
		extendperms(Perms1,X,Perms2),
		insert_one_item(Perm,X,[],Perms2,Perms3).

	insert_one_item([],_,_,Perms,Perms).
	insert_one_item([Y|Ys],X,Acc,Perms1,[Zs|Perms2]) :-
		revapp(Acc,[Y,X|Ys],Zs),
		insert_one_item(Ys,X,[Y|Acc],Perms1,Perms2).

	revapp([],Acc,Acc).
	revapp([X|Xs],Acc,Zs) :-
		revapp(Xs,[X|Acc],Zs).

	nats(Max,Max,[Max]) :-
		!.
	nats(Curr,Max,[Curr|Ns]) :-
		Curr<Max,
		Curr1 is Curr+1,
		nats(Curr1,Max,Ns).

	permute([],[]).
	permute([X|Xs],Zs) :-
		permute(Xs,Ys),
		ins(X,Ys,Zs).

	ins(X,Ys,[X|Ys]).
	ins(X,[Y|Ys],[Y|Zs]) :-
		ins(X,Ys,Zs).

	backtracking(N) :-
		nats(1, N, Ns),
		permute(Ns, _),
		fail.
	backtracking(_).

	list(N, Ps) :-
		nats(1, N, Ns),
		allperms(Ns,Ps).

	all(N, Ps) :-
		nats(1, N, Ns),
		findall(P, permute(Ns,P), Ps).

	map(N, Qs) :-
		nats(1, N, Ns),
		allperms(Ns, Ps),
		meta::map(copy_term, Ps, Qs).

	copy(N) :-
		nats(1, N, Ns),
		allperms(Ns, Ps),
		list::member(P, Ps),
		copy_term(P, _),
		fail.
	copy(_).

:- end_object.
