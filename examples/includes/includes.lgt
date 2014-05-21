%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- include(includes('vowels.pl')).


:- object(countries).

	:- public([
		capitals/1,
		same_population/1
	]).

	:- include(includes('countries.pl')).

	capitals(Capitals) :-
		setof(Capital, Country^Population^country(Country, Capital, Population), Capitals).

	same_population(Countries) :-
		setof(Country, Capital^country(Country, Capital, _Population), Countries).

:- end_object.
