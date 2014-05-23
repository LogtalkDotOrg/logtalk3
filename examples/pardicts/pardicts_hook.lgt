%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(pardicts_hook,
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/05/22,
		comment is 'Access to an object (or category) first parameter as a SWI-Prolog dictionary.']).

	goal_expansion(
		parameter_create(Pairs),
		(parameter(1, Dict), dict_create(Dict, p, Pairs))
	).
	goal_expansion(
		get_parameter(Key, Value),
		(parameter(1, Dict), get_dict(Key, Dict, Value))
	).
	goal_expansion(
		b_set_parameter(Key, Value),
		(parameter(1, Dict), b_set_dict(Key, Dict, Value))
	).
	goal_expansion(
		nb_set_parameter(Key, Value),
		(parameter(1, Dict), nb_set_dict(Key, Dict, Value))
	).

:- end_object.
