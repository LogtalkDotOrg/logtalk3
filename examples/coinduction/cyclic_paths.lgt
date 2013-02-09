%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(cp1).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/24,
		comment is 'Coinduction example of finding the cyclic paths in a graph.'
	]).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, b).	arc(b, c).
	arc(c, d).	arc(c, a).
	arc(d, d).

:- end_object.



:- object(cp2).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/24,
		comment is 'Coinduction example of finding the cyclic paths in a graph.'
	]).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, c).
	arc(c, a).	arc(c, d).
	arc(d, a).

:- end_object.



:- object(cp3).

	:- info([
		version is 1.0,
		author is 'Paulo Moura. Derived from a Gopal Gupta et al example.',
		date is 2012/07/30,
		comment is 'Coinduction example of finding the cyclic paths in a graph.'
	]).

	:- public(path/3).

	path(From, Path, MaxLength) :-
		path(From, Path, 0, MaxLength).

	:- private(path/4).
	:- coinductive(path(+, +, -, -)).

	path(From, [From| Path], Length, MaxLength) :-
		Length < MaxLength,
		Length1 is Length + 1,
		arc(From, Next),
		path(Next, Path, Length1, MaxLength).

	arc(a, b).
	arc(b, c).
	arc(c, a).	arc(c, d).
	arc(d, a).

:- end_object.
