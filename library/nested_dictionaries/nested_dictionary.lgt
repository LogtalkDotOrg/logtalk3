%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paul Brown <pbrown@optimusprime.ai> and
%                      Paulo Moura <pmoura@logtalk.org>
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


	new(Dict) :-
		^^new(Dict).

	empty(Dict) :-
		^^empty(Dict).

	as_nested_dictionary(Term, Dict) :-
		(	var(Term) ->
			instantiation_error
		;	nonvar(Dict) ->
			uninstantiation_error(Dict)
		;	functor(Term, '{}', Arity), Arity =< 1,
			curly_to_dictionary(Term, Dict) ->
			true
		;	type_error(curly_bracketed_term, Term)
		).

	curly_to_dictionary(Value, Value) :-
		(	var(Value)
		;	atomic(Value), Value \== {}
		;	compound(Value), Value \= {_}, Value \= [_|_]
		),
		!.
	curly_to_dictionary({}, Dict) :-
		^^new(Dict).
	curly_to_dictionary({Term}, Dict) :-
		^^new(Empty),
		pairs_to_dictionary(Term, Empty, Dict).
	curly_to_dictionary([Element| Elements], [Dict| Dicts]) :-
		curly_to_dictionary(Element, Dict),
		curly_to_dictionary(Elements, Dicts).

	pairs_to_dictionary(':'(Key, Value), Acc, Dict) :-
		curly_to_dictionary(Value, DictValue),
		^^insert(Acc, Key, DictValue, Dict).
	pairs_to_dictionary((':'(Key, Value), Pairs), Acc, Dict) :-
		curly_to_dictionary(Value, DictValue),
		^^insert(Acc, Key, DictValue, Updated),
		pairs_to_dictionary(Pairs, Updated, Dict).

	pairs_to_dictionary(Key-Value, Acc, Dict) :-
		curly_to_dictionary(Value, DictValue),
		^^insert(Acc, Key, DictValue, Dict).
	pairs_to_dictionary((Key-Value, Pairs), Acc, Dict) :-
		curly_to_dictionary(Value, DictValue),
		^^insert(Acc, Key, DictValue, Updated),
		pairs_to_dictionary(Pairs, Updated, Dict).

	as_curly_bracketed(Dict, Term) :-
		(	var(Dict)	->
			instantiation_error
		;	nonvar(Term) ->
			uninstantiation_error(Term)
		;	^^check(Dict),
			^^as_list(Dict, Pairs),
			pairs_to_curly(Pairs, Term)
		).

	pairs_to_curly([], {}).
	pairs_to_curly([Pair| Pairs], {Term}) :-
		pairs_to_curly(Pairs, Pair, Term).

	pairs_to_curly([], Key-Value, Key-TermValue) :-
		value_to_term_value(Value, TermValue).
	pairs_to_curly([Pair| Pairs], Key-Value, (Key-TermValue, TermPairs)) :-
		value_to_term_value(Value, TermValue),
		pairs_to_curly(Pairs, Pair, TermPairs).

	value_to_term_value(Value, _) :-
		var(Value),
		instantiation_error.
	value_to_term_value([], []) :-
		!.
	value_to_term_value([Value| Values], [TermValue| TermValues]) :-
		!,
		value_to_term_value(Value, TermValue),
		value_to_term_value(Values, TermValues).
	value_to_term_value(Value, Value) :-
		\+ ^^valid(Value),
		!.
	value_to_term_value(Value, TermValue) :-
		^^as_list(Value, Pairs),
		pairs_to_curly(Pairs, TermValue).

	lookup_in([], Dict, Dict).
	lookup_in([Key| Keys], Value, Dict) :-
		^^lookup(Key, SubDict, Dict),
		lookup_in(Keys, Value, SubDict).

	update_in(OldDict, [Key| Keys], Value, NewDict) :-
		update_in(Keys, OldDict, Key, _, Value, NewDict).

	update_in(OldDict, [Key| Keys], OldValue, NewValue, NewDict) :-
		update_in(Keys, OldDict, Key, OldValue, NewValue, NewDict).

	update_in([], OldDict, Key, OldValue, NewValue, NewDict) :-
		^^update(OldDict, Key, OldValue, NewValue, NewDict).
	update_in([NextKey| Keys], OldDict, Key, OldValue, NewValue, NewDict) :-
		^^lookup(Key, SubDict, OldDict),
		^^update(OldDict, Key, NewSubDict, NewDict),
		update_in(Keys, SubDict, NextKey, OldValue, NewValue, NewSubDict).

	insert_in(OldDict, [Key| Keys], Value, NewDict) :-
        insert_in(Keys, OldDict, Key, Value, NewDict).

    insert_in([], OldDict, Key, Value, NewDict) :-
        ^^insert(OldDict, Key, Value, NewDict).
    insert_in([NextKey| Keys], OldDict, Key, Value, NewDict) :-
        ^^lookup(Key, SubDict, OldDict),
        ^^update(OldDict, Key, NewDict0, NewDict),
        insert_in(Keys, SubDict, NextKey, Value, NewDict0).

	delete_in(OldDict, [Key| Keys], Value, NewDict) :-
		delete_in(Keys, OldDict, Key, Value, NewDict).

	delete_in([], OldDict, Key, Value, NewDict) :-
		^^delete(OldDict, Key, Value, NewDict).
	delete_in([NextKey| Keys], OldDict, Key, Value, NewDict) :-
        ^^lookup(Key, SubDict, OldDict),
        ^^update(OldDict, Key, NewDict0, NewDict),
		delete_in(Keys, SubDict, NextKey, Value, NewDict0).
