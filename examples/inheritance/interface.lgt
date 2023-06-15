%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


/*
This category defines a predicate, interface/0, that prints an object
interface, i.e. predicate names and the corresponding scope properties.

We need to encapsulate the interface/0 predicate in a category instead
of just defining it in a root object in order to be able to list private
object predicates.
*/

:- category(interface).

	:- public(interface/0).

	interface :-
		% list all predicates visible in "this" (except this one)
		current_predicate(Name/Arity),
			functor(Pred, Name, Arity),
			Pred \== interface,
			predicate_property(Pred, scope(Scope)),
			writeq(Name/Arity), write(' - '), writeq(Scope), nl,
		fail.
	interface.

:- end_category.
