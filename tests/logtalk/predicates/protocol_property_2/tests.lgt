%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/12/16,
		comment is 'Unit tests for the protocol_property/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(protocol_property_2_1, error(type_error(protocol_identifier, 1), logtalk(protocol_property(1, static), _))) :-
		protocol_property(1, static).

	throws(protocol_property_2_2, error(type_error(callable, 1), logtalk(protocol_property(monitoring, 1), _))) :-
		protocol_property(monitoring, 1).

	throws(protocol_property_2_3, error(domain_error(protocol_property, foo), logtalk(protocol_property(monitoring, foo), _))) :-
		protocol_property(monitoring, foo).

	fails(protocol_property_2_4) :-
		protocol_property(non_exisiting_protocol, _).

	fails(protocol_property_2_5) :-
		protocol_property(monitoring, (dynamic)).

	succeeds(protocol_property_2_6) :-
		findall(Prop, protocol_property(monitoring, Prop), _).

	% entity info
	succeeds(protocol_property_2_7) :-
		protocol_property(test_protocol, static),
		protocol_property(test_protocol, file(Basename, Directory)), ground(Basename), ground(Directory),
		protocol_property(test_protocol, lines(Start, End)), integer(Start), integer(End),
		protocol_property(test_protocol, info(Info)),
		member(version(_), Info),
		member(author(_), Info),
		member(date(_), Info),
		member(comment(_), Info).

	% entity interface
	succeeds(protocol_property_2_8) :-
		protocol_property(test_protocol, public(Public)), Public == [a/1],
		protocol_property(test_protocol, protected(Protected)), Protected == [b/2],
		protocol_property(test_protocol, private(Private)), Private == [c/3].

	% interface predicate declaration properties
	succeeds(protocol_property_2_9) :-
		protocol_property(test_protocol, declares(a/1, Properties1)),
		member((public), Properties1),
		member(scope(Scope1), Properties1), Scope1 == (public),
		member(static, Properties1),
		(	current_logtalk_flag(coinduction, supported) ->
			member(coinductive(Template), Properties1), Template == a((+))
		;	true
		),
		member(line_count(LC1), Properties1), integer(LC1),
		protocol_property(test_protocol, declares(b/2, Properties2)),
		member(protected, Properties2),
		member(scope(Scope2), Properties2), Scope2 == protected,
		member(static, Properties2),
		(	current_logtalk_flag(threads, supported) ->
			member(synchronized, Properties2)
		;	true
		),
		member(line_count(LC2), Properties2), integer(LC2),
		protocol_property(test_protocol, declares(c/3, Properties3)),
		member(private, Properties3),
		member(scope(Scope3), Properties3), Scope3 == private,
		member((dynamic), Properties3),
		member(line_count(LC3), Properties3), integer(LC3).

	member(H, [H| _]) :-
		!.
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
