%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


bar :-					% test predicate
	write('bar predicate called'), nl.


:- object(bypass).

	:- public(foo//0).
	:- mode(foo, one).
	:- info(foo//0, [
		comment is 'Just the almighty and famous old foo.'
	]).

	foo --> {{bar}}.	% the external pair of {}'s have the usual DCG semantics;
						% the internal pair of {}'s is the Logtalk compiler bypass control construct
:- end_object.
