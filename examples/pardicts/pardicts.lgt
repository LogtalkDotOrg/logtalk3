%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(obj(_Dict_)).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2018/12/17,
		comment is 'Simple object for testing passing a dict as the object parameter.'
	]).

	:- public(sum/1).
	sum(Sum) :-
		get_dict(m, _Dict_, M),
		get_dict(n, _Dict_, N),
		Sum is M + N.

	:- public(product/1).
	product(Product) :-
		get_dict(m, _Dict_, M),
		get_dict(n, _Dict_, N),
		Product is M * N.

	:- public(double/0).
	double :-
		get_dict(m, _Dict_, M),
		M2 is M * 2,
		b_set_dict(m, _Dict_, M2),
		get_dict(n, _Dict_, N),
		N2 is N * 2,
		b_set_dict(n, _Dict_, N2).

:- end_object.
