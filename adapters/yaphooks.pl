%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
%
%  integration code for YAP 6.0.2 and later versions to improve
%  usability when using the YAP profilers
%  Last updated on November 27, 2012
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- op(600, xfy, ::).	% message-sending operator


:- multifile(user:prolog_predicate_name/2).

user:prolog_predicate_name(user:'$lgt_send_to_obj_'(_, _, _), '::/2 (event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne_'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self_'(_, _, _), '::/1') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_'(_, _, _), '^^/2 (from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_'(_, _, _), '^^/2 (from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_call_'(_, _, _, _), ':/1') :- !.
user:prolog_predicate_name(user:'$lgt_call_in_this'(_, _), 'call/1') :- !.

user:prolog_predicate_name(user:'$lgt_send_to_obj'(_, _, _), '::/2 (event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne'(_, _, _), '::/2 (event transparent)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self'(_, _, _), '::/1') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call'(_, _, _), '^^/2 (from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call'(_, _, _), '^^/2 (from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_call'(_, _, _), ':/1') :- !.

user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_this'(_, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_sender'(_, _, _), 'call/N') :- !.

user:prolog_predicate_name(user:'$lgt_expand_term'(_, _, _, _, _), 'expand_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_expand_goal'(_, _, _, _, _), 'expand_goal/2') :- !.

user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _), 'phrase/2') :- !.
user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _, _), 'phrase/3') :- !.

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
	Goal \= _::_,
	(	Goal = Module:THead ->
		Module == user
	;	Goal = THead
	),
	functor(THead, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, Functor/Arity),
	(	atom(Entity) ->
		atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
	;	functor(Entity, EFunctor, EArity),
		atomic_list_concat([EFunctor, '/', EArity, '::', Functor, '/', Arity], Label)
	).
