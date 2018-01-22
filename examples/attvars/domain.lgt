%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


% example adapted from the SWI-Prolog documentation on attributed variables

:- object(domain).

	:- public(domain/2).

	:- uses(set, [insert_all/3, intersection/3, memberchk/2, new/1]).

	domain(X, Domain) :-
		var(Domain),
		!,
		get_attr(X, domain, Domain).
	domain(X, List) :-
		new(Set),
		insert_all(List, Set, Domain),
		put_attr(Y, domain, Domain),
		X = Y.

	% Var, ?Domain
	%	An attributed variable with attribute value Domain has been
	%	assigned the value Y
	attr_unify_hook(Domain, Y) :-
		(	var(Y) ->
			(	get_attr(Y, domain, DomainY) ->
				intersection(Domain, DomainY, NewDomain),
				(	NewDomain == [] ->
					fail
				;	NewDomain = [Value] ->
					Y = Value
				;	put_attr(Y, domain, NewDomain)
				)
			;	put_attr(Y, domain, Domain)
			)
		;	memberchk(Y, Domain)
		).

	% Translate attributes from this module to residual goals
	attribute_goals(X) -->
		{ get_attr(X, domain, Domain) },
		[domain(X, Domain)].

:- end_object.
