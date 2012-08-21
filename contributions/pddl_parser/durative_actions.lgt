
:- category(durative_actions).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/06/12,
		comment is 'Durative action grammar rules for parsing PDDL 3.0 domains.']).

	durative_action_def -->
		['(', ':', 'durative-action'], da_symbol,
		[':', 'parameters'], 
		da_def_body, [')'].

	da_def_body -->
		[':', 'duration'], duration_constraint,
		[':', 'condition'], emptyOr(da_gd),
		[':', 'effect'], emptyOr(da_effect).		

	da_symbol --> name.

	da_gd --> pref_timed_gd.
	da_gd --> ['(', and], zeroOrMore(da_gd, T), [')'].
	da_gd --> pref_timed_gd.	

	pref_timed_gd --> timed_gd.
	pref_timed_gd --> ['(', preference, ], pref_name, timed_gd, [')'].

	timed_gd --> ['(', at], time_specifier, gd, [')'].
	timed_gd --> ['(', over], interval, gd, [')'].

	time_specifier --> [start].
	time_specifier --> [end].

	interval --> [all].

:- end_category.
