%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(shared_paths).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/29,
		comment is 'Shared cycles coinductive example.']).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, c).
	arc(c, d).	arc(c, f).
	arc(d, e).
	arc(e, f).
	arc(f, a).	arc(f, c).

:- end_object.
