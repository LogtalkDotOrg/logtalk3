
:- set_prolog_flag(double_quotes, atom).


:- object(tests_atom,
	extends(lgtunit)).

	:- info([
		version is 1:9:0,
		author is 'Paulo Moura',
		date is 2021-11-15,
		comment is 'Unit tests for the de facto Prolog standard format/3 built-in predicate with format strings specified using atoms.'
	]).

	:- include(tests).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	% workaround XSB atom-based module system
	:- import(from(/(format,2), format)).
:- endif.
