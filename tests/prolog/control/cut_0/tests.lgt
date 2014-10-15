%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.4.4

twice(!) :- write('C ').
twice(true) :- write('Moss ').

goal((twice(_), !)).
goal(write('Three ')).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard !/0 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.4.4

	succeeds(iso_cut_0_01) :-
		{!}.

	fails(iso_cut_0_02) :-
		{(!,fail;true)}.

	succeeds(iso_cut_0_03) :-
		{(call(!),fail;true)}.

	fails(iso_cut_0_04) :-
		{(twice(_), !, write('Forwards '), fail)}.

	fails(iso_cut_0_05) :-
		{((!; write('No ')), write('Cut disjunction '), fail)}.

	fails(iso_cut_0_06) :-
		{(twice(_), (write('No '); !), write('Cut '), fail)}.

	fails(iso_cut_0_07) :-
		{(twice(_), (!, fail; write('No ')))}.

	fails(iso_cut_0_08) :-
		{(twice(X), call(X), write('Forwards '), fail)}.

	fails(iso_cut_0_09) :-
		{(goal(X), call(X), write('Forwards '), fail)}.

	fails(iso_cut_0_10) :-
		{(twice(_), \+(\+(!)), write('Forwards '), fail)}.

	fails(iso_cut_0_11) :-
		{(twice(_), once(!), write('Forwards '), fail)}.

	fails(iso_cut_0_12) :-
		{(twice(_), call(!), write('Forwards '), fail)}.


:- end_object.
