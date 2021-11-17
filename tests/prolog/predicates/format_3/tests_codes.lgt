
:- set_prolog_flag(double_quotes, codes).


:- object(tests_codes,
	extends(lgtunit)).

	:- info([
		version is 1:10:0,
		author is 'Paulo Moura',
		date is 2021-11-17,
		comment is 'Unit tests for the de facto Prolog standard format/3 built-in predicate with format strings specified using lists of codes.'
	]).

	:- include(tests).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	% workaround XSB atom-based module system
	:- import(from(/(format,2), format)).
:- endif.
