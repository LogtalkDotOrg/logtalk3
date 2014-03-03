%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(parsetree,
	implements(parsep)).

	parse(List, Tree) :-
		phrase(sentence(Tree), List).

	sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

	noun_phrase(np(D,NP)) --> determiner(D), noun(NP).
	noun_phrase(NP) --> noun(NP).

	verb_phrase(vp(V)) --> verb(V).
	verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).

	determiner(d(the)) --> [the].
	determiner(d(a)) --> [a].

	noun(n(boy)) --> [boy].
	noun(n(girl)) --> [girl].

	verb(v(likes)) --> [likes].
	verb(v(hates)) --> [hates].

:- end_object.
