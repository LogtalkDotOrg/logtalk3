%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% we start by defining three hook objects; each hook object defines a
% term_expansion/2 clause that expands a fact, a term_expansion/2 clause
% that adds a new clause to the expanded object, and a goal_expansion/2
% that rewrites a goal

:- object(ha,
	implements(expanding)).		% hook objects should always reference
								% the "expanding" built-in protocol
	term_expansion(a, [aa]).
	term_expansion((:- end_object), [ha, (:- end_object)]).

	goal_expansion(start, first).

:- end_object.


:- object(hb,
	implements(expanding)).

	% term_expansion/2 clauses can return a list of terms
	term_expansion(b, [bb]).
	term_expansion((:- end_object), [hb, (:- end_object)]).

	goal_expansion(first, second).

:- end_object.


:- object(hc,
	implements(expanding)).

	% term_expansion/2 clauses may also return a single term
	term_expansion(c, cc).
	term_expansion((:- end_object), [hc, (:- end_object)]).

	goal_expansion(second, true).

:- end_object.


% we can also combine different hook objects by defining a new hook object
% whose term_expansion/2 and goal_expansion/2 clauses reference the
% term_expansion/2 and goal_expansion/2 clauses in other hook objects

:- object(hh,
	implements(expanding)).

	% the term-expansion mechanism tries term_expansion/2 clauses until
	% it finds one that succeeds (or until all term_expansion/2 clauses
	% are tried); thus more specific clauses should be listed before more
	% general ones

	term_expansion((:- object(raw)), (:- object(cooked))).

	% the next term_expansion/2 clause returns the result of all hook
	% expansions, assumed to be independent of each other, as the final
	% expansion of a single term

	term_expansion((:- end_object), Expansion) :-
		ha::term_expansion((:- end_object), HA0),
		list::append(HA, [(:- end_object)], HA0),
		hb::term_expansion((:- end_object), HB0),
		list::append(HB, [(:- end_object)], HB0),
		hc::term_expansion((:- end_object), HC0),
		list::append(HC, [(:- end_object)], HC0),
		list::append([HA, HB, HC, [(:- end_object)]], Expansion).

	% here we just try each individual hook object in succesion but more
	% elaborate combining schemes could be implemented if necessary

	term_expansion(Term, Expansion) :-
		ha::term_expansion(Term, Expansion).
	term_expansion(Term, Expansion) :-
		hb::term_expansion(Term, Expansion).
	term_expansion(Term, Expansion) :-
		hc::term_expansion(Term, Expansion).

	% the following goal_expansion/2 clause defines a pipeline where
	% the result of one hook expansion is feed into the next hook

	goal_expansion(start, Expansion) :-
		ha::goal_expansion(start, Expansion0),
		hb::goal_expansion(Expansion0, Expansion1),
		hc::goal_expansion(Expansion1, Expansion).

:- end_object.
