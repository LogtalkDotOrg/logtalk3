%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(client, [test/0, test/1, names/0, names/1]).

:- use_module(lists, [contained/2]).
:- use_module(metapreds, [meta/1]).

names :-
	contained(P, [paulo, carlos, helena]),
	write(P), nl,
	fail.
names.

names(Names) :-
	findall(Name, contained(Name, [paulo, carlos, helena]), Names).

test :-
	meta(names).

test(Names) :-
	meta(names(Names)).
