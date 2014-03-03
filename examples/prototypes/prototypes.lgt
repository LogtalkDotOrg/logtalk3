%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Alf believes he is the only survivor of his species; no point in
% defining a class if there is only going to be a single instance:

:- object(alf).	% a prototype, which is also a stand-alone object

	% prototypes declare predicates for themselves (and derived prototypes)
	:- public(
		[name/1, planet/1, stomachs/1, favorite_food/1, chases/1, motto/1]).

	name('Gordon Shumway').
	planet('Melmac').
	stomachs(8).
	favorite_food(cats).
	chases('Lucky').
	motto('Are you going to finish that sandwich?').

:- end_object.


% later on, Alf finds out that his best friend, Skip, and his
% girlfriend, Rhonda, also survived Melmac's explosion; as they
% are all melmacians, they share most attributes (and add some
% of their own):

:- object(skip,		% derived prototype
	extends(alf)).	% parent prototype

	:- public(best_friend/1).

	best_friend(alf).
	name('Skip').
	chases(_) :-	% still longing for a nice cat
		fail.		% to eat since Melmac exploded

:- end_object.


:- object(rhonda,	% derived prototype
	extends(alf)).	% parent prototype

	:- public(boyfriend/1).

	boyfriend(alf).
	name('Rhonda').
	chases(_) :-	% still longing for a nice cat
		fail.		% to eat since Melmac exploded

:- end_object.
