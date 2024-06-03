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


:- object(default_compound_mutations).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-23,
		comment is 'Default compound mutations.',
		see_also is [type]
	]).

	% mutate the functor name
	mutation(compound, Compound, Mutation) :-
		Compound =.. [Name| Arguments],
		mutations_store::mutation(atom, Name, NameMutation),
		Mutation =.. [NameMutation| Arguments].
	% mutate the arguments
	mutation(compound, Compound, Mutation) :-
		Compound =.. [Name| Arguments],
		mutations_store::mutation(list, Arguments, ArgumentsMutation),
		Mutation =.. [Name| ArgumentsMutation].

:- end_object.
