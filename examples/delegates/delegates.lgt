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


% define a category for holding the interface
% and implementation for delegator objects

:- category(delegator).

	:- public(delegate/1).
	:- public(set_delegate/1).

	:- private(delegate_/1).
	:- dynamic(delegate_/1).

	delegate(Delegate) :-
		::delegate_(Delegate).

	set_delegate(Delegate) :-
		::retractall(delegate_(Delegate)),
		::assertz(delegate_(Delegate)).

:- end_category.

% define a simple delegator object, with a
% method, operation/1, for testing delegation

:- object(a_delegator,
	imports(delegator)).

	:- public(operation/1).

	operation(String) :-
		(	::delegate(Delegate), Delegate::current_predicate(thing/1) ->
			% a delegate is defined that understands the method thing/1
			Delegate::thing(String)
		;	% otherwise just use the default implementation
			String = 'default implementation'
		).

:- end_object.

% an alternative definition for a simple delegator object
% is to use a parametric object whose parameter would be
% instantiated to the delegate object:

:- object(a_delegator(_Delegate)).

	:- public(operation/1).

	operation(String) :-
		(	parameter(1, Delegate), Delegate::current_predicate(thing/1) ->
			% a delegate is defined that understands the method thing/1
			Delegate::thing(String)
		;	% otherwise just use the default implementation
			String = 'default implementation'
		).

:- end_object.

% define an interface for delegate objects 

:- protocol(delegate).

	:- public(thing/1).

:- end_protocol.

% define a simple delegate

:- object(a_delegate,
	implements(delegate)).

	thing('delegate implementation').

:- end_object.

% define a simple object that doesn't implement the "delegate" interface

:- object(an_object).

:- end_object.

% test the delegation solution when this file is compiled and loaded
/*
:- initialization((
	% without a delegate:
	a_delegator::operation(String1),
	String1 == 'default implementation',
	% with a delegate that does not implement thing/1:
	a_delegator::set_delegate(an_object),
	a_delegator::operation(String2),
	String2 == 'default implementation',
	% with a delegate that implements thing/1:
	a_delegator::set_delegate(a_delegate),
	a_delegator::operation(String3),
	String3 == 'delegate implementation'
)).
*/
