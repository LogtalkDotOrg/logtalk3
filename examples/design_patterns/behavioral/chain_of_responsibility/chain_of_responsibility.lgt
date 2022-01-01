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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Chain-of-responsibility design pattern:
%
% https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern


% the Wikipedia example deals with purchasing requests that need approval;
% depending on the purchase amount, the request may need to be forwarded up
% in a company hierarchy


% use a category for the purchase power protocol and main predicate; as
% categories cannot receive messages, they can play a similar role to an
% abstract class

:- category(purchase_power).

	% allowable amount that can be approved
	% by a given person role 
	:- public(allowable/1).

	% main client predicate handling purchase requests
	:- public(process_request/1).
	
	process_request(Amount) :-
		::allowable(Allowable),
		(	Amount < Allowable ->
			::role(Role),
			write(Role), write(' will approve '), write(Amount), nl
		;	::successor(Successor) ->
			[Successor::process_request(Amount)]
		;	write('Request denied for '), write(Amount), nl,
			fail
		).

	% person role so that we may know who approved a request
	:- public(role/1).

	% base value for computing allowable amounts per person role
	:- private(base/1).
	base(500).

	% successor in the chain of responsibility when the current
	% object cannot handle a request
	:- private(successor/1).

:- end_category.


% define four person roles, each one delegating non-handled
% requests to the next person in the chain of responsibility

:- object(manager,
	imports(purchase_power)).

	allowable(Allowable) :-
		^^base(Base),
		Allowable is Base * 10.

	role('Manager').

	successor(director).

:- end_object.


:- object(director,
	imports(purchase_power)).

	allowable(Allowable) :-
		^^base(Base),
		Allowable is Base * 20.

	role('Director').

	successor(vice_president).

:- end_object.


:- object(vice_president,
	imports(purchase_power)).

	allowable(Allowable) :-
		^^base(Base),
		Allowable is Base * 40.

	role('Vice President').

	successor(president).

:- end_object.


:- object(president,
	imports(purchase_power)).

	allowable(Allowable) :-
		^^base(Base),
		Allowable is Base * 60.

	role('President').

:- end_object.
