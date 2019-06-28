%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


% Predicate scope directives specify predicate *calling* semantics, not
% *definition* semantics. For example, if an object declares a private
% predicate, the object can call the predicate locally but also call it
% in the object descendants.


:- object(prototype).

	:- public(foo/1).
	% any object (or category) can send a prototype::foo/1 message;
	% descendant objects can redefine this predicate and call inherited
	% definitions using ^^foo/1; this object can access definitions for
	% this predicate in descendant objects by sending a ::bar/1 message
	foo(1).

	:- protected(bar/1).
	% no object (other than this one) can send a prototype::bar/1 message;
	% descendant objects can redefine this predicate and call inherited
	% definitions using ^^bar/1; this object can access definitions for
	% this predicate in descendant objects by sending a ::bar/1 message
	bar(1).

	:- private(baz/1).
	% no object (other than this one) can send a prototype::baz/1 message;
	% descendant objects can redefine this predicate but cannot access any
	% inherited definition using ^^baz/1; this object can call this
	% predicate in descendant objects using ::baz/1
	baz(1).

	% this predicate can only be called locally; it's invisible to
	% the current_predicate/1 and predicate_property/2 reflection
	% predicates, which only list predicates with scope directives
	local(1).

	:- public(p_foo/1).
	% access any overridden definition in this object descendants
	p_foo(X) :-
		::foo(X).

	:- public(p_bar/1).
	% access any overridden definition in this object descendants
	p_bar(X) :-
		::bar(X).

	:- public(p_baz/1).
	% access any overridden definition in this object descendants
	p_baz(X) :-
		::baz(X).

:- end_object.


:- object(descendant,
	extends(prototype)).

	% redefined predicates
	foo(2).

	bar(2).

	baz(2).

	% unrelated with the local/1 local predicate in "prototype"
	local(2).

	:- public(d_foo/1).
	% access any inherited definition for a public predicate
	d_foo(X) :-
		^^foo(X).

	:- public(d_bar/1).
	% access any inherited definition for a protected predicate
	d_bar(X) :-
		^^bar(X).

:- end_object.
