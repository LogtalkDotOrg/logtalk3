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
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "roots" example.'
	]).

	test(roots_1) :-
		object::ancestors(Ancestors),
		list::msort(Ancestors, AncestorsSorted),
		AncestorsSorted == [abstract_class, class, object].

	test(roots_2) :-
		class::instances(Instances),
		list::msort(Instances, InstancesSorted),
		InstancesSorted == [abstract_class, class, object].

	test(roots_3) :-
		findall(Super, class::superclass(Super), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [abstract_class, object].

:- end_object.
