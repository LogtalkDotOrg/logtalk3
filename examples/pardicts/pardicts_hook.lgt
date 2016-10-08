%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
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
