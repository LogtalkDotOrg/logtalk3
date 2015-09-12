%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(attvars_hook,
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/03/13,
		comment is 'Hook object for compiling objects and categories using attributed variables.'
	]).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		term_expansion((attr_unify_hook(Att, Var) :- Body), [{(:- multifile(attr_unify_hook/3))}, ({attr_unify_hook(Var, Prefix, _-Att)} :- Body)]) :-
			logtalk_load_context(entity_prefix, Prefix).
		term_expansion((attribute_goals(_) --> _), []).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		term_expansion((attr_unify_hook(Att, Var) :- Body), [{(:- import(from(/(install_verify_attribute_handler,4), machine)))},{(:- install_verify_attribute_handler(Prefix,Att,Var,TAttrUnifyHookHead))},(attr_unify_hook(_-Att, Var) :- Body)]) :-
			logtalk_load_context(entity_prefix, Prefix),
			logtalk::compile_predicate_heads(attr_unify_hook(Att, Var), _, TAttrUnifyHookHead, _).
		term_expansion((attribute_goals(X) --> Body), [{(:- import(from(/(install_attribute_portray_hook,3), machine)))},{(:- install_attribute_portray_hook(Prefix,X,TAttrUnifyHookHead))},(attribute_goals(X) --> Body)]) :-
			logtalk_load_context(entity_prefix, Prefix),
			logtalk::expand_term((attribute_goals(X) --> Body), (AttrUnifyHookHead :- _)),
			logtalk::compile_predicate_heads(AttrUnifyHookHead, _, TAttrUnifyHookHead, _).

	:- else.

		term_expansion((attr_unify_hook(Att, Var) :- Body), [(:- multifile(Prefix:attr_unify_hook/2)), (Prefix:attr_unify_hook(_-Att, Var) :- Body)]) :-
			logtalk_load_context(entity_prefix, Prefix).
		term_expansion((attribute_goals(X) --> Body), [(:- multifile(Prefix:attribute_goals//1)), (Prefix:attribute_goals(X) --> Body)]) :-
			logtalk_load_context(entity_prefix, Prefix).

	:- endif.

	goal_expansion(get_attr(Var, Entity, Value), get_attr(Var, Prefix, Parameters-Value)) :-
		entity_to_prefix_and_parameters(Entity, Prefix, Parameters).
	goal_expansion(put_attr(Var, Entity, Value), put_attr(Var, Prefix, Parameters-Value)) :-
		entity_to_prefix_and_parameters(Entity, Prefix, Parameters).
	goal_expansion(del_attr(Var, Entity), del_attr(Var, Prefix)) :-
		entity_to_prefix_and_parameters(Entity, Prefix, _).

	entity_to_prefix_and_parameters(Entity, Prefix, Parameters) :-
		callable(Entity),
		(	logtalk_load_context(entity_identifier, Entity) ->
			% reference to entity under compilation
			logtalk_load_context(entity_prefix, Prefix)
		;	% reference to other entity; try to avoid expansion loop
			\+ logtalk::entity_prefix(_, Entity),
			logtalk::entity_prefix(Entity, Prefix)
		),
		Entity =.. [_| Parameters].

:- end_object.
