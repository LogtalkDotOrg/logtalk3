%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(folds).

	:- info([
		version is 1:0:0,
		date is 2018-08-20,
		author is 'Paulo Moura',
		comment is 'Usage examples of the folds meta-predicates.'
	]).

	:- public(left/1).
	left(Left) :-
		meta::fold_left(my_atom_concat, '0', ['1','2','3','4','5','6','7','8','9'], Left).

	:- public(right/1).
	right(Right) :-
		meta::fold_right(my_atom_concat, '0', ['1','2','3','4','5','6','7','8','9'], Right).

	my_atom_concat(X, Y, Z) :-
		atom_concat('(', X, Z0),
		atom_concat(Z0, '+', Z1),
		atom_concat(Z1, Y, Z2),
		atom_concat(Z2, ')', Z).

:- end_object.
