%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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

	%	Translate attributes from this module to residual goals
	attribute_goals(X) -->
		{ get_attr(X, domain, Domain) },
		[domain(X, Domain)].

:- end_object.
