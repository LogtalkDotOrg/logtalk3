%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration code for YAP 6.3.4 and later versions to improve
%  usability when using the YAP profilers.
%  Last updated on August 29, 2014
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


:- multifile(user:prolog_predicate_name/2).

user:prolog_predicate_name(user:'$lgt_send_to_obj_'(_, _, _), '::/2 (event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne_'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self_'(_, _, _), '::/1') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_'(_, _, _), '^^/2 (from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_'(_, _, _), '^^/2 (from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_call_in_this'(_, _), 'call/1') :- !.

user:prolog_predicate_name(user:'$lgt_send_to_obj_rt'(_, _, _, _), '::/2 (runtime)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj'(_, _, _), '::/2 (event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self'(_, _, _), '::/1') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call'(_, _, _), '^^/2 (from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call'(_, _, _), '^^/2 (from ctg; same pred)') :- !.

user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall'(_, _), 'call/1') :- !.
user:prolog_predicate_name(user:'$lgt_quantified_metacall'(_, _, _), 'call/1') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_local'(_, _), 'call/1') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_sender'(_, _, _, _), 'call/1') :- !.

user:prolog_predicate_name(user:'$lgt_bagof'(_, _, _, _, _), 'bagof/3') :- !.
user:prolog_predicate_name(user:'$lgt_setof'(_, _, _, _, _), 'setof/3') :- !.

user:prolog_predicate_name(user:'$lgt_expand_term'(_, _, _, _, _), 'expand_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_expand_goal'(_, _, _, _, _), 'expand_goal/2') :- !.

user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _), 'phrase/2') :- !.
user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _, _), 'phrase/3') :- !.

user:prolog_predicate_name(user:'$lgt_compiler_flag'(_, _), 'current_logtalk_flag/2') :- !.
user:prolog_predicate_name(user:'$lgt_set_compiler_flag'(_, _), 'set_logtalk_flag/2') :- !.

user:prolog_predicate_name(user:'$lgt_current_op'(_, _, _, _, _, _), 'current_op/3') :- !.
user:prolog_predicate_name(user:'$lgt_current_predicate'(_, _, _, _), 'current_predicate/1') :- !.
user:prolog_predicate_name(user:'$lgt_predicate_property'(_, _, _, _, _), 'predicate_property/2') :- !.

user:prolog_predicate_name(user:'$lgt_abolish_checked'(_, _, _, _), 'abolish/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_fact_checked'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_rule_checked'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_fact_checked'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_rule_checked'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_clause_checked'(_, _, _, _, _), 'clause/2') :- !.
user:prolog_predicate_name(user:'$lgt_retract_fact_checked'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retract_rule_checked'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retractall_checked'(_, _, _, _), 'retractall/1') :- !.

user:prolog_predicate_name(user:'$lgt_iso_read_term'(_, _, _, _), 'read_term/3') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read_term'(_, _, _), 'read_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read'(_, _, _), 'read/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read'(_, _), 'read/1') :- !.

user:prolog_predicate_name(user:'$lgt_iso_write_term'(_, _, _, _), 'write_term/3') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write_term'(_, _, _), 'write_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write'(_, _, _), 'write/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write'(_, _), 'write/1') :- !.
user:prolog_predicate_name(user:'$lgt_iso_writeq'(_, _, _), 'writeq/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_writeq'(_, _), 'writeq/1') :- !.

user:prolog_predicate_name(user:'$lgt_category_parameter'(_, _, _, _), 'parameter/2') :- !.

user:prolog_predicate_name(user:'$lgt_threaded_or'(_, _, _), 'threaded/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_and'(_, _, _), 'threaded/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_ignore'(_), 'threaded_ignore/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call'(_, _, _), 'threaded_call/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call'(_, _, _, _), 'threaded_call/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once'(_, _, _), 'threaded_once/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once'(_, _, _, _), 'threaded_once/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call_tagged'(_, _, _, _), 'threaded_call/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call_tagged'(_, _, _, _, _), 'threaded_call/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once_tagged'(_, _, _, _), 'threaded_once/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once_tagged'(_, _, _, _, _), 'threaded_once/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek'(_, _, _, _), 'threaded_peek/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek'(_, _, _, _, _), 'threaded_peek/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek_tagged'(_, _, _, _, _), 'threaded_peek/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek_tagged'(_, _, _, _, _, _), 'threaded_peek/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit'(_, _, _, _), 'threaded_exit/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit'(_, _, _, _, _), 'threaded_exit/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit_tagged'(_, _, _, _, _), 'threaded_exit/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit_tagged'(_, _, _, _, _, _), 'threaded_exit/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_synch_ctg'(_, _, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_synch'(_, _, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_ctg'(_, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait'(_, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_notify_ctg'(_, _), 'threaded_notify/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_notify'(_, _), 'threaded_notify/1') :- !.

user:prolog_predicate_name(Goal, Label) :-
	Goal \= '::'(_, _),
	(	Goal = Module:THead ->
		Module == user
	;	Goal = THead
	),
	functor(THead, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, _, Functor/Arity),
	(	atom(Entity) ->
		atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
	;	functor(Entity, EFunctor, EArity),
		atomic_list_concat([EFunctor, '/', EArity, '::', Functor, '/', Arity], Label)
	).
