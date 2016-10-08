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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/09/27,
		comment is 'Unit tests for the category/3 opening directive.'
	]).

	% test all possible syntaxes for category relations

	test(category_0) :-
		logtalk_load(categories, [unknown_entities(silent)]).

	test(category_1) :-
		implements_protocol(category_1, protocol1),
		extends_category(category_1, parent1),
		complements_object(category_1, object1).

	test(category_2) :-
		implements_protocol(category_2, protocol1),
		implements_protocol(category_2, protocol2),
		extends_category(category_2, parent1),
		extends_category(category_2, parent2),
		complements_object(category_2, object1),
		complements_object(category_2, object2).

	test(category_3) :-
		implements_protocol(category_3, protocol1),
		implements_protocol(category_3, protocol2),
		extends_category(category_3, parent1),
		extends_category(category_3, parent2),
		complements_object(category_3, object1),
		complements_object(category_3, object2).

:- end_object.
