%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Tests for the "json_ld" library.'
	]).

	:- uses(json_ld, [
		parse/2, generate/2, expand/2, compact/3, flatten/2, frame/3
	]).

	:- uses(list, [
		length/2, valid/1 as is_list/1
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	cover(json_ld).
	cover(json_ld(_)).
	cover(json_ld(_, _, _)).

	cleanup :-
		^^clean_file('test_files/output.jsonld'),
		^^clean_file('test_files/output2.jsonld').

	% =============== Error Handling Tests ===============

	test(json_ld_parse_error_var_source, error(instantiation_error)) :-
		parse(_, _).

	test(json_ld_parse_error_invalid_source, error(domain_error(json_ld_source, _))) :-
		parse(invalid(foo), _).

	test(json_ld_generate_error_var_sink, error(instantiation_error)) :-
		generate(_, {}).

	test(json_ld_generate_error_invalid_sink, error(domain_error(json_ld_sink, _))) :-
		generate(invalid(foo), {}).

	test(json_ld_expand_error_var_document, error(instantiation_error)) :-
		expand(_, _).

	test(json_ld_compact_error_var_document, error(instantiation_error)) :-
		compact(_, {}, _).

	test(json_ld_compact_error_var_context, error(instantiation_error)) :-
		compact({}, _, _).

	test(json_ld_frame_error_var_document, error(instantiation_error)) :-
		frame(_, {}, _).

	test(json_ld_frame_error_var_frame, error(instantiation_error)) :-
		frame({}, _, _).

	% =============== Parsing Tests ===============

	test(json_ld_parse_from_file, deterministic) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		parse(file(Path), Term),
		assertion(ground(Term)).

	test(json_ld_parse_from_atom, deterministic(Term == {'@id'-'http://example.org/1'})) :-
		parse(atom('{"@id":"http://example.org/1"}'), Term).

	test(json_ld_parse_from_codes, deterministic) :-
		atom_codes('{"@type":"Person"}', Codes),
		parse(codes(Codes), Term),
		assertion(Term == {'@type'-'Person'}).

	test(json_ld_parse_from_chars, deterministic) :-
		atom_chars('{"name":"test"}', Chars),
		parse(chars(Chars), Term),
		assertion(Term == {name-test}).

	test(json_ld_parse_simple_person, deterministic) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		parse(file(Path), Term),
		assertion(Term = {'@context'-_, name-'Manu Sporny', homepage-'http://manu.sporny.org/', image-'http://manu.sporny.org/images/manu.png'}).

	test(json_ld_parse_graph, deterministic) :-
		^^file_path('test_files/graph.jsonld', Path),
		parse(file(Path), Term),
		assertion(Term = {'@context'-_, '@graph'-_}).

	test(json_ld_parse_expanded, deterministic) :-
		^^file_path('test_files/expanded.jsonld', Path),
		parse(file(Path), Term),
		assertion(is_list(Term)).

	test(json_ld_parse_empty_context, deterministic) :-
		^^file_path('test_files/empty_context.jsonld', Path),
		parse(file(Path), _Term).

	% =============== Generation Tests ===============

	test(json_ld_generate_to_atom, deterministic) :-
		generate(atom(Atom), {'@id'-'http://example.org/1'}),
		assertion(atom(Atom)).

	test(json_ld_generate_to_codes, deterministic) :-
		generate(codes(Codes), {'@id'-'http://example.org/1'}),
		assertion(is_list(Codes)).

	test(json_ld_generate_to_chars, deterministic) :-
		generate(chars(Chars), {'@id'-'http://example.org/1'}),
		assertion(is_list(Chars)).

	test(json_ld_roundtrip_atom, deterministic(Term == {'@id'-'http://example.org/1', name-test})) :-
		generate(atom(Atom), {'@id'-'http://example.org/1', name-test}),
		parse(atom(Atom), Term).

	% =============== Expansion Tests: Core Features ===============

	test(json_ld_expand_simple_context, deterministic) :-
		parse(atom('{"@context":{"name":"http://schema.org/name"},"name":"Manu"}'), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'http://schema.org/name'-[{'@value'-'Manu'}]}]).

	test(json_ld_expand_id, deterministic) :-
		parse(atom('{"@context":{"name":"http://schema.org/name"},"@id":"http://example.org/1","name":"Test"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node == {'@id'-'http://example.org/1', 'http://schema.org/name'-[{'@value'-'Test'}]}).

	test(json_ld_expand_type_single, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/"},"@type":"Person"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-['http://schema.org/Person']}).

	test(json_ld_expand_type_multiple, deterministic) :-
		^^file_path('test_files/multiple_types.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', '@type'-['http://schema.org/Person', 'http://schema.org/Author'], _}).

	test(json_ld_expand_vocab, deterministic) :-
		^^file_path('test_files/context_vocab.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-['http://schema.org/Person'], 'http://schema.org/name'-_, 'http://schema.org/jobTitle'-_}).

	test(json_ld_expand_base, deterministic) :-
		^^file_path('test_files/context_base.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/person/1', 'http://schema.org/name'-_}).

	test(json_ld_expand_base_parent_path, deterministic(Expanded == [{'@id'-'http://example.org/a/d'}])) :-
		expand({'@context'-{'@base'-'http://example.org/a/b/c'}, '@id'-'../d'}, Expanded).

	test(json_ld_expand_base_query_reference, deterministic(Expanded == [{'@id'-'http://example.org/a/b?q=1'}])) :-
		expand({'@context'-{'@base'-'http://example.org/a/b?old#frag'}, '@id'-'?q=1'}, Expanded).

	test(json_ld_expand_base_fragment_reference, deterministic(Expanded == [{'@id'-'http://example.org/a/b?old#new'}])) :-
		expand({'@context'-{'@base'-'http://example.org/a/b?old#frag'}, '@id'-'#new'}, Expanded).

	test(json_ld_expand_compact_iri, deterministic) :-
		^^file_path('test_files/compact_iri.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://xmlns.com/foaf/0.1/name'-_, 'http://purl.org/dc/terms/title'-_}).

	% =============== Expansion Tests: Value Objects ===============

	test(json_ld_expand_typed_value, deterministic) :-
		^^file_path('test_files/typed_value.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://purl.org/dc/terms/modified'-_}).

	test(json_ld_expand_language_tagged, deterministic) :-
		^^file_path('test_files/language_tagged.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/name'-_, 'http://schema.org/description'-_}).

	test(json_ld_expand_default_language, deterministic) :-
		^^file_path('test_files/default_language.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-_, 'http://schema.org/name'-_, 'http://schema.org/author'-_}).

	% =============== Expansion Tests: Graph and Structure ===============

	test(json_ld_expand_graph, deterministic) :-
		^^file_path('test_files/graph.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@graph'-_}).

	test(json_ld_expand_reverse, deterministic) :-
		^^file_path('test_files/reverse_property.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://schema.org/name'-_, '@reverse'-_}).

	test(json_ld_expand_list, deterministic) :-
		^^file_path('test_files/list.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'@id'-'http://example.org/people/1', 'http://xmlns.com/foaf/0.1/nick'-[{'@list'-[{'@value'-'joe'}, {'@value'-'bob'}, {'@value'-'jaybee'}]}]}]).

	test(json_ld_expand_list_null, deterministic(Expanded == [{'http://example.org/items'-[{'@list'-[]}]}])) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}, items-{'@list'- @null}}, Expanded).

	test(json_ld_expand_list_of_lists, error(domain_error(json_ld_list_of_lists, _))) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}, items-{'@list'-[[a]]}}, _).

	test(json_ld_expand_set, deterministic) :-
		^^file_path('test_files/set.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'@id'-'http://example.org/people/1', 'http://xmlns.com/foaf/0.1/nick'-[{'@set'-[{'@value'-'joe'}, {'@value'-'bob'}]}]}]).

	test(json_ld_expand_included, deterministic) :-
		^^file_path('test_files/included.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/collection', '@type'-_, '@included'-_}).

	test(json_ld_expand_index, deterministic) :-
		^^file_path('test_files/index.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', '@type'-_, 'http://schema.org/name'-_, '@index'-'person-index'}).

	test(json_ld_expand_blank_node, deterministic) :-
		^^file_path('test_files/blank_node.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'_:b0', 'http://schema.org/name'-_, 'http://schema.org/knows'-_}).

	% =============== Expansion Tests: Multiple Contexts ===============

	test(json_ld_expand_multiple_contexts, deterministic) :-
		^^file_path('test_files/multiple_contexts.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/name'-_, 'http://schema.org/url'-_}).

	% =============== Expansion Tests: Nested Objects ===============

	test(json_ld_expand_nested_objects, deterministic) :-
		^^file_path('test_files/nested_objects.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-_, 'http://schema.org/name'-_, 'http://schema.org/knows'-_}).

	test(json_ld_expand_schema_org_event, deterministic) :-
		^^file_path('test_files/schema_org_event.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-_, 'http://schema.org/name'-_, 'http://schema.org/startDate'-_, 'http://schema.org/endDate'-_, 'http://schema.org/location'-_, 'http://schema.org/organizer'-_}).

	% =============== Expansion Tests: Type Coercion ===============

	test(json_ld_expand_type_coercion_id, deterministic) :-
		^^file_path('test_files/type_coercion.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/name'-_, 'http://schema.org/url'-_}).

	test(json_ld_expand_expanded_term_def, deterministic) :-
		^^file_path('test_files/expanded_term_definition.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://schema.org/name'-_, 'http://schema.org/age'-_, 'http://schema.org/url'-_}).

	% =============== Expansion Tests: Scalars and Special Values ===============

	test(json_ld_expand_boolean_true, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"active":true}'), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'http://example.org/active'-[{'@value'- @true}]}]).

	test(json_ld_expand_boolean_false, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"active":false}'), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'http://example.org/active'-[{'@value'- @false}]}]).

	test(json_ld_expand_null_value, deterministic(Expanded == [])) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"prop":null}'), Doc),
		expand(Doc, Expanded).

	test(json_ld_expand_number_integer, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"count":42}'), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'http://example.org/count'-[{'@value'-42}]}]).

	test(json_ld_expand_number_float, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"value":3.14}'), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'http://example.org/value'-[{'@value'-3.14}]}]).

	test(json_ld_expand_array_values, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"tags":["a","b","c"]}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/tags'-_}).

	% =============== Expansion Tests: Already-Expanded Form ===============

	test(json_ld_expand_already_expanded_array, deterministic) :-
		^^file_path('test_files/expanded.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% =============== Expansion Tests: Inline Atom Sources ===============

	test(json_ld_expand_empty_object, deterministic(Expanded == [])) :-
		expand({}, Expanded).

	test(json_ld_expand_no_context, deterministic) :-
		expand({'http://schema.org/name'-'Test'}, Expanded),
		assertion(Expanded == [{'http://schema.org/name'-[{'@value'-'Test'}]}]).

	% =============== Compaction Tests ===============

	test(json_ld_compact_simple, deterministic) :-
		Context = {'@context'-{name-'http://schema.org/name'}},
		expand({'@context'-{name-'http://schema.org/name'}, name-'Manu'}, Expanded),
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	test(json_ld_compact_with_vocab, deterministic) :-
		Context = {'@context'-{'@vocab'-'http://schema.org/'}},
		expand({'@context'-{'@vocab'-'http://schema.org/'}, '@type'-'Person', name-'Jane'}, Expanded),
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	test(json_ld_compact_empty_list, deterministic(Compacted == [])) :-
		compact([], {'@context'-{}}, Compacted).

	% =============== Parametric Object Tests ===============

	test(json_ld_parse_curly_dash_atom, deterministic) :-
		json_ld(curly, dash, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == {'@id'-'http://example.org/1'}).

	test(json_ld_parse_list_dash_atom, deterministic) :-
		json_ld(list, dash, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == json(['@id'-'http://example.org/1'])).

	test(json_ld_parse_list_equal_atom, deterministic) :-
		json_ld(list, equal, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == json(['@id'='http://example.org/1'])).

	test(json_ld_parse_list_colon_atom, deterministic) :-
		json_ld(list, colon, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == json([':'('@id','http://example.org/1')])).

	test(json_ld_parse_curly_dash_chars, deterministic) :-
		json_ld(curly, dash, chars)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == {chars(['@',i,d])-chars([h,t,t,p,:,/,/,e,x,a,m,p,l,e,'.',o,r,g,/,'1'])}).

	test(json_ld_shorthand_object_1, deterministic) :-
		json_ld(atom)::parse(atom('{"name":"test"}'), Term),
		assertion(Term == {name-test}).

	test(json_ld_shorthand_object_0, deterministic) :-
		json_ld::parse(atom('{"name":"test"}'), Term),
		assertion(Term == {name-test}).

	% =============== Expansion with List Representation ===============

	test(json_ld_expand_list_representation, deterministic) :-
		json_ld(list, dash, atom)::parse(atom('{"@context":{"name":"http://schema.org/name"},"name":"Test"}'), Doc),
		json_ld(list, dash, atom)::expand(Doc, Expanded),
		assertion(Expanded == [json(['http://schema.org/name'-[json(['@value'-'Test'])]])]).

	% =============== Roundtrip Tests ===============

	test(json_ld_roundtrip_file, deterministic) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		parse(file(Path), Term),
		generate(atom(Atom), Term),
		assertion(atom(Atom)),
		parse(atom(Atom), Term2),
		assertion(ground(Term2)).

	test(json_ld_roundtrip_expand_compact, deterministic) :-
		Doc = {'@context'-{name-'http://schema.org/name'}, name-'Manu'},
		Context = {'@context'-{name-'http://schema.org/name'}},
		expand(Doc, Expanded),
		compact(Expanded, Context, _Compacted).

	% =============== Direction Tests ===============

	test(json_ld_expand_direction, deterministic) :-
		^^file_path('test_files/direction.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% =============== Additional Coverage Tests ===============

	% expand_document/3 clause 1: empty list
	test(json_ld_expand_empty_list, deterministic(Expanded == [])) :-
		expand([], Expanded).

	% expand_document/3 clause 4: scalar value
	test(json_ld_expand_scalar_value, deterministic(Expanded == [42])) :-
		expand(42, Expanded).

	% expand_document/3 clause 4: string value
	test(json_ld_expand_string_value, deterministic(Expanded == [hello])) :-
		expand(hello, Expanded).

	% compact_document/3 clause 4: scalar value
	test(json_ld_compact_scalar_value, deterministic(Compacted == 42)) :-
		compact(42, {'@context'-{}}, Compacted).

	% compact_value_array/3 clause 1: empty array
	test(json_ld_compact_value_empty_array, deterministic) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}, items-[]}, Expanded),
		compact(Expanded, {'@context'-{'@vocab'-'http://example.org/'}}, Compacted),
		assertion(ground(Compacted)).

	% compact_value_array/3 clause 3: multi-element array
	test(json_ld_compact_value_multi_array, deterministic) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}, items-[1, 2, 3]}, Expanded),
		compact(Expanded, {'@context'-{'@vocab'-'http://example.org/'}}, Compacted),
		assertion(ground(Compacted)).

	% process_context_entry/4: @direction handling
	test(json_ld_context_direction, deterministic) :-
		parse(atom('{"@context":{"@direction":"rtl","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	test(json_ld_context_invalid_base, error(domain_error(json_ld_context_base, _))) :-
		parse(atom('{"@context":{"@base":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	test(json_ld_context_invalid_vocab, error(domain_error(json_ld_context_vocab, _))) :-
		parse(atom('{"@context":{"@vocab":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	test(json_ld_context_invalid_language, error(domain_error(json_ld_context_language, _))) :-
		parse(atom('{"@context":{"@language":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	test(json_ld_context_invalid_direction, error(domain_error(json_ld_context_direction, _))) :-
		parse(atom('{"@context":{"@direction":"sideways","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	% process_context_entry/4: @version handling (ignored)
	test(json_ld_context_version, deterministic) :-
		parse(atom('{"@context":{"@version":1.1,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	test(json_ld_context_invalid_version, error(domain_error(json_ld_context_version, _))) :-
		parse(atom('{"@context":{"@version":1.0,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	% process_context_entry/4: @propagate handling
	test(json_ld_context_propagate, deterministic) :-
		parse(atom('{"@context":{"@propagate":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	test(json_ld_context_invalid_propagate, error(domain_error(json_ld_context_propagate, _))) :-
		parse(atom('{"@context":{"@propagate":"yes","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	test(json_ld_context_invalid_propagate_prolog_true, error(domain_error(json_ld_context_propagate, _))) :-
		expand({'@context'-{'@propagate'-true, name-'http://schema.org/name'}, name-'test'}, _).

	% process_context_entry/4: @protected handling
	test(json_ld_context_protected, deterministic) :-
		parse(atom('{"@context":{"@protected":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	test(json_ld_context_invalid_protected, error(domain_error(json_ld_context_protected, _))) :-
		parse(atom('{"@context":{"@protected":"yes","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	test(json_ld_context_invalid_protected_prolog_false, error(domain_error(json_ld_context_protected, _))) :-
		expand({'@context'-{'@protected'-false, name-'http://schema.org/name'}, name-'test'}, _).

	% process_context_entry/4: @import requires an external document loader
	test(json_ld_context_import, error(domain_error(json_ld_context_import, _))) :-
		parse(atom('{"@context":{"@import":"http://example.org/context","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, _).

	% process_context_entry/4: term definition to null
	test(json_ld_context_term_null, deterministic) :-
		parse(atom('{"@context":{"name":null,"title":"http://schema.org/title"},"title":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% remove_context_value/3: @vocab to null (tests context array processing)
	test(json_ld_context_vocab_null, deterministic) :-
		parse(atom('{"@context":[{"@vocab":"http://schema.org/"},{"@vocab":null}],"http://schema.org/name":"test"}'), Doc),
		expand(Doc, Expanded),
		% The full IRI property should still expand normally
		assertion(is_list(Expanded)).

	% remove_context_value/3: @base to null
	test(json_ld_context_base_null, deterministic) :-
		parse(atom('{"@context":[{"@base":"http://example.org/"},{"@base":null}],"@id":"foo"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% remove_context_value/3: @language to null
	test(json_ld_context_language_null, deterministic) :-
		parse(atom('{"@context":[{"@language":"en"},{"@language":null}],"http://schema.org/name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% make_value_object_id/2: @type: @id coercion
	test(json_ld_type_coercion_id, deterministic) :-
		parse(atom('{"@context":{"homepage":{"@id":"http://schema.org/url","@type":"@id"}},"homepage":"http://example.org/"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/url'-[{'@id'-'http://example.org/'}]}).

	% make_value_object_id/2: @type: @vocab coercion
	test(json_ld_type_coercion_vocab, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/","status":{"@type":"@vocab"}},"status":"Active"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/status'-[{'@id'-'http://schema.org/Active'}]}).

	% make_typed_value_object/3: typed literal coercion
	test(json_ld_type_coercion_typed_literal, deterministic) :-
		parse(atom('{"@context":{"age":{"@id":"http://schema.org/age","@type":"http://www.w3.org/2001/XMLSchema#integer"}},"age":"30"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/age'-[{'@value'-'30', '@type'-'http://www.w3.org/2001/XMLSchema#integer'}]}).

	% find_prefix_for_iri/4: prefix:suffix compaction
	test(json_ld_compact_prefix_iri, deterministic) :-
		Expanded = [{'http://xmlns.com/foaf/0.1/name'-[{'@value'-'Alice'}]}],
		Context = {'@context'-{foaf-'http://xmlns.com/foaf/0.1/'}},
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	% pairs_to_object/2 clause 1: empty pairs
	test(json_ld_expand_object_no_properties, deterministic) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}}, Expanded),
		assertion(Expanded == []).

	% parse/2 clause 3: stream parsing
	test(json_ld_parse_from_stream, deterministic) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		open(Path, read, Stream),
		parse(stream(Stream), Term),
		close(Stream),
		assertion(ground(Term)).

	% generate/2 clause 2: file generation
	test(json_ld_generate_to_file, deterministic) :-
		^^file_path('test_files/output.jsonld', Path),
		generate(file(Path), {'@id'-'http://example.org/1'}),
		parse(file(Path), Term),
		assertion(Term == {'@id'-'http://example.org/1'}).

	% generate/2 clause 3: stream generation
	test(json_ld_generate_to_stream, deterministic) :-
		^^file_path('test_files/output2.jsonld', Path),
		open(Path, write, Stream),
		generate(stream(Stream), {'@id'-'http://example.org/1'}),
		close(Stream),
		parse(file(Path), Term),
		assertion(Term == {'@id'-'http://example.org/1'}).

	% pair_key_value/3 with equal representation
	test(json_ld_expand_equal_representation, deterministic) :-
		json_ld(list, equal, atom)::expand(json(['@context'={'@vocab'='http://example.org/'}, name='Test']), Expanded),
		assertion(is_list(Expanded)).

	% pair_key_value/3 with colon representation
	test(json_ld_expand_colon_representation, deterministic) :-
		json_ld(list, colon, atom)::expand(json([':'('@context',':'('@vocab','http://example.org/')), ':'(name,'Test')]), Expanded),
		assertion(is_list(Expanded)).

	% expand_pair/4 clause 13: @nest (ignored)
	test(json_ld_expand_nest, deterministic) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/"},"@nest":{"name":"Test"}}'), Doc),
		expand(Doc, Expanded),
		assertion(Expanded == [{'http://schema.org/name'-[{'@value'-'Test'}]}]).

	test(json_ld_expand_nest_array, deterministic(Expanded == [{'http://schema.org/name'-[{'@value'-'Test'}], 'http://schema.org/url'-[{'@id'-'http://example.org/'}]}])) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/","url":{"@type":"@id"}},"@nest":[{"name":"Test"},{"url":"http://example.org/"}]}'), Doc),
		expand(Doc, Expanded).

	test(json_ld_expand_nest_invalid_value, error(domain_error(json_ld_nest_value, _))) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/"},"@nest":"bad"}'), Doc),
		expand(Doc, _).

	% String representations are normalized for algorithm keys/IRIs but preserved as values
	test(json_ld_expand_chars_value, deterministic) :-
		json_ld(curly, dash, chars)::parse(atom('{"@context":{"@vocab":"http://example.org/"},"name":"test"}'), Doc),
		json_ld(curly, dash, chars)::expand(Doc, Expanded),
		assertion(Expanded == [{'http://example.org/name'-[{'@value'-chars([t,e,s,t])}]}]).

	test(json_ld_expand_codes_value, deterministic) :-
		json_ld(curly, dash, codes)::parse(atom('{"@context":{"@vocab":"http://example.org/"},"name":"test"}'), Doc),
		json_ld(curly, dash, codes)::expand(Doc, Expanded),
		assertion(Expanded == [{'http://example.org/name'-[{'@value'-codes([116,101,115,116])}]}]).

	% expand_iri/4 clause 9: fallback for unresolvable IRI
	test(json_ld_expand_unresolvable_iri, deterministic) :-
		parse(atom('{"@context":{},"unknownProp":"value"}'), Doc),
		expand(Doc, Expanded),
		% unknownProp should remain unexpanded and get filtered out
		assertion(is_list(Expanded)).

	% Keyword expansion coverage - ensure @json is tested
	test(json_ld_keyword_json, deterministic) :-
		parse(atom('{"@context":{"data":{"@id":"http://example.org/data","@type":"@json"}},"data":{"key":"value"}}'), Doc),
		expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% Type coercion with list values - expand_list_elements/4 with type coercion
	test(json_ld_type_coercion_id_list, deterministic) :-
		parse(atom('{"@context":{"links":{"@id":"http://schema.org/links","@type":"@id"}},"links":["http://example.org/a","http://example.org/b"]}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/links'-[{'@id'-'http://example.org/a'}, {'@id'-'http://example.org/b'}]}).

	% find_prefix_for_iri/4 with multiple prefixes - tests clause 3 (recursive)
	test(json_ld_compact_multiple_prefixes, deterministic) :-
		Expanded = [{'http://purl.org/dc/terms/title'-[{'@value'-'Title'}], 'http://xmlns.com/foaf/0.1/name'-[{'@value'-'Name'}]}],
		Context = {'@context'-{dc-'http://purl.org/dc/terms/', foaf-'http://xmlns.com/foaf/0.1/'}},
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	% Compact with term definition in context - term def entries have compound keys that
	% are filtered by atom(Key) check, not separate is_term_def_key predicate
	test(json_ld_compact_with_term_def, deterministic) :-
		Expanded = [{'http://schema.org/name'-[{'@value'-'Alice'}]}],
		Context = {'@context'-{name-{'@id'-'http://schema.org/name'}}},
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	% Compact IRI not matching term, uses prefix search
	test(json_ld_compact_skip_term_def_key, deterministic) :-
		Expanded = [{'http://xmlns.com/foaf/0.1/homepage'-[{'@id'-'http://example.org/'}]}],
		% Context has both term def and prefix
		Context = {'@context'-{foaf-'http://xmlns.com/foaf/0.1/', knows-{'@id'-'http://xmlns.com/foaf/0.1/knows'}}},
		compact(Expanded, Context, Compacted),
		% Should compact to foaf:homepage using prefix (not matching knows term)
		assertion(ground(Compacted)).

	% Type coercion with @vocab in nested context
	test(json_ld_type_coercion_vocab_nested, deterministic) :-
		parse(atom('{"@context":[{"@vocab":"http://schema.org/"},{"status":{"@type":"@vocab"}}],"status":"Active"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/status'-[{'@id'-'http://schema.org/Active'}]}).

	% =============== Flatten Tests ===============

	% Error handling test for flatten
	test(json_ld_flatten_error_var_document, error(instantiation_error)) :-
		flatten(_, _).

	% Flatten a simple expanded node
	test(json_ld_flatten_simple_node, deterministic(Flattened == [{'@id'-'http://example.org/john', 'http://schema.org/name'-[{'@value'-'John'}]}])) :-
		Expanded = [{'@id'-'http://example.org/john', 'http://schema.org/name'-[{'@value'-'John'}]}],
		flatten(Expanded, Flattened).

	% Flatten nested nodes - nested node should be extracted
	test(json_ld_flatten_nested_nodes, deterministic(Flattened == [
			{'@id'-'http://example.org/john', 'http://schema.org/knows'-[{'@id'-'http://example.org/jane'}]},
			{'@id'-'http://example.org/jane', 'http://schema.org/name'-[{'@value'-'Jane'}]}
		])) :-
		Expanded = [{'@id'-'http://example.org/john',
			'http://schema.org/knows'-[{'@id'-'http://example.org/jane', 'http://schema.org/name'-[{'@value'-'Jane'}]}]}],
		flatten(Expanded, Flattened).

	% Flatten generates blank node IDs for nodes without @id
	test(json_ld_flatten_blank_node_generation, deterministic(Flattened == [{'@id'-'_:b0', 'http://schema.org/name'-[{'@value'-'Anonymous'}]}])) :-
		Expanded = [{'http://schema.org/name'-[{'@value'-'Anonymous'}]}],
		flatten(Expanded, Flattened).

	% Flatten an empty expanded document
	test(json_ld_flatten_empty, deterministic(Flattened == [])) :-
		flatten([], Flattened),
		true.

	% Flatten multiple top-level nodes
	test(json_ld_flatten_multiple_nodes, deterministic(Flattened == [{'@id'-'http://example.org/b'}, {'@id'-'http://example.org/a'}])) :-
		Expanded = [{'@id'-'http://example.org/a'}, {'@id'-'http://example.org/b'}],
		flatten(Expanded, Flattened).

	% Flatten merges nodes with same @id
	test(json_ld_flatten_merge_same_id, deterministic(Flattened == [{'@id'-'http://example.org/a', 'http://schema.org/name'-[{'@value'-'Name'}], 'http://schema.org/age'-[{'@value'-30}]}])) :-
		% Two nodes with same @id but different properties
		Expanded = [{'@id'-'http://example.org/a', 'http://schema.org/name'-[{'@value'-'Name'}]},
			{'@id'-'http://example.org/a', 'http://schema.org/age'-[{'@value'-30}]}],
		flatten(Expanded, Flattened).

	% Flatten preserves @type
	test(json_ld_flatten_preserves_type, deterministic(Flattened == [{'@id'-'http://example.org/john', '@type'-['http://schema.org/Person']}])) :-
		Expanded = [{'@id'-'http://example.org/john', '@type'-['http://schema.org/Person']}],
		flatten(Expanded, Flattened).

	% Test flattening through parse and expand workflow
	test(json_ld_flatten_workflow, deterministic(Flattened == [{'@id'-'http://example.org/test', 'http://schema.org/name'-[{'@value'-'Test'}]}])) :-
		parse(atom('{"@context":{"name":"http://schema.org/name"},"@id":"http://example.org/test","name":"Test"}'), Doc),
		expand(Doc, Expanded),
		flatten(Expanded, Flattened),
		true.

	% Flatten named graphs into graph nodes
	test(json_ld_flatten_named_graph, deterministic(Flattened == [{'@id'-'http://example.org/graph', '@graph'-[{'@id'-'http://example.org/a', 'http://schema.org/name'-[{'@value'-'A'}]}]}])) :-
		Expanded = [{'@id'-'http://example.org/graph', '@graph'-[{'@id'-'http://example.org/a', 'http://schema.org/name'-[{'@value'-'A'}]}]}],
		flatten(Expanded, Flattened).

	% Flatten preserves lists while extracting node objects inside them
	test(json_ld_flatten_list, deterministic(Flattened == [
			{'@id'-'http://example.org/collection', 'http://schema.org/items'-[{'@list'-[{'@id'-'http://example.org/item'}, {'@value'-1}]}]},
			{'@id'-'http://example.org/item', 'http://schema.org/name'-[{'@value'-'Item'}]}
		])) :-
		Expanded = [{'@id'-'http://example.org/collection', 'http://schema.org/items'-[{'@list'-[{'@id'-'http://example.org/item', 'http://schema.org/name'-[{'@value'-'Item'}]}, {'@value'-1}]}]}],
		flatten(Expanded, Flattened).

	% Flatten reverse properties onto the referenced nodes
	test(json_ld_flatten_reverse_properties, deterministic(Flattened == [
			{'@id'-'http://example.org/bob', 'http://schema.org/knows'-[{'@id'-'http://example.org/alice'}], 'http://schema.org/name'-[{'@value'-'Bob'}]},
			{'@id'-'http://example.org/alice'}
		])) :-
		Expanded = [{'@id'-'http://example.org/alice', '@reverse'-{'http://schema.org/knows'-[{'@id'-'http://example.org/bob', 'http://schema.org/name'-[{'@value'-'Bob'}]}]}}],
		flatten(Expanded, Flattened).

	% Flatten included blocks into the active graph
	test(json_ld_flatten_included, deterministic(Flattened == [
			{'@id'-'http://example.org/item', 'http://schema.org/name'-[{'@value'-'Item'}]},
			{'@id'-'http://example.org/collection'}
		])) :-
		Expanded = [{'@id'-'http://example.org/collection', '@included'-[{'@id'-'http://example.org/item', 'http://schema.org/name'-[{'@value'-'Item'}]}]}],
		flatten(Expanded, Flattened).

	% Flatten suppresses duplicate values
	test(json_ld_flatten_duplicate_suppression, deterministic(Flattened == [
			{'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'@id'-'http://example.org/b'}]},
			{'@id'-'http://example.org/b'}
		])) :-
		Expanded = [{'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'@id'-'http://example.org/b'}, {'@id'-'http://example.org/b'}]}],
		flatten(Expanded, Flattened).

	% Flatten merges type arrays while preserving first-seen order
	test(json_ld_flatten_type_merging, deterministic(Flattened == [{'@id'-'http://example.org/a', '@type'-['http://schema.org/A', 'http://schema.org/B']}])) :-
		Expanded = [{'@id'-'http://example.org/a', '@type'-['http://schema.org/A']}, {'@id'-'http://example.org/a', '@type'-['http://schema.org/B', 'http://schema.org/A']}],
		flatten(Expanded, Flattened).

	% Flatten consistently relabels referenced blank node identifiers
	test(json_ld_flatten_blank_node_relabeling, deterministic(Flattened == [
			{'@id'-'_:b0', 'http://schema.org/name'-[{'@value'-'Blank'}]},
			{'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'@id'-'_:b0'}]}
		])) :-
		Expanded = [{'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'@id'-'_:x'}, {'@id'-'_:x'}]}, {'@id'-'_:x', 'http://schema.org/name'-[{'@value'-'Blank'}]}],
		flatten(Expanded, Flattened).

	% Flatten rejects conflicting @index values for the same node
	test(json_ld_flatten_conflicting_indexes, error(domain_error(json_ld_conflicting_indexes, _))) :-
		Expanded = [{'@id'-'http://example.org/a', '@index'-'one'}, {'@id'-'http://example.org/a', '@index'-'two'}],
		flatten(Expanded, _).

	% =============== Framing Tests ===============

	% Frame nodes by @type
	test(json_ld_frame_type, deterministic(Framed == [{'@id'-'http://example.org/alice', '@type'-['http://schema.org/Person'], 'http://schema.org/name'-[{'@value'-'Alice'}]}])) :-
		Document = [{'@id'-'http://example.org/alice', '@type'-['http://schema.org/Person'], 'http://schema.org/name'-[{'@value'-'Alice'}]}, {'@id'-'http://example.org/acme', '@type'-['http://schema.org/Organization']}],
		Frame = {'@type'-'http://schema.org/Person'},
		frame(Document, Frame, Framed).

	% Frame nodes by @id
	test(json_ld_frame_id, deterministic(Framed == [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}]}])) :-
		Document = [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}]}, {'@id'-'http://example.org/bob'}],
		Frame = {'@id'-'http://example.org/alice'},
		frame(Document, Frame, Framed).

	% Frame and compact when the frame carries a context
	test(json_ld_frame_compacted_context, deterministic(Framed == {'@context'-{name-'http://schema.org/name'}, '@id'-'http://example.org/alice', name-'Alice'})) :-
		Document = [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}]}],
		Frame = {'@context'-{name-'http://schema.org/name'}, '@id'-'http://example.org/alice'},
		frame(Document, Frame, Framed).

	% Nested frames embed referenced nodes by default
	test(json_ld_frame_nested_embed, deterministic(Framed == [{'@id'-'http://example.org/alice', 'http://schema.org/knows'-[{'@id'-'http://example.org/bob', 'http://schema.org/name'-[{'@value'-'Bob'}]}]}])) :-
		Document = [{'@id'-'http://example.org/alice', 'http://schema.org/knows'-[{'@id'-'http://example.org/bob'}]}, {'@id'-'http://example.org/bob', 'http://schema.org/name'-[{'@value'-'Bob'}]}],
		Frame = {'@id'-'http://example.org/alice', 'http://schema.org/knows'-[{}]},
		frame(Document, Frame, Framed).

	% @embed false keeps references instead of embedding node objects
	test(json_ld_frame_embed_false, deterministic(Framed == [{'@id'-'http://example.org/alice', 'http://schema.org/knows'-[{'@id'-'http://example.org/bob'}]}])) :-
		Document = [{'@id'-'http://example.org/alice', 'http://schema.org/knows'-[{'@id'-'http://example.org/bob'}]}, {'@id'-'http://example.org/bob', 'http://schema.org/name'-[{'@value'-'Bob'}]}],
		Frame = {'@id'-'http://example.org/alice', 'http://schema.org/knows'-[{'@embed'- @false}]},
		frame(Document, Frame, Framed).

	test(json_ld_frame_invalid_embed_prolog_false, error(domain_error(json_ld_framing_boolean, _))) :-
		Document = [{'@id'-'http://example.org/alice'}],
		Frame = {'@id'-'http://example.org/alice', '@embed'-false},
		frame(Document, Frame, _).

	% @explicit true only includes properties present in the frame
	test(json_ld_frame_explicit, deterministic(Framed == [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}]}])) :-
		Document = [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}], 'http://schema.org/age'-[{'@value'-30}]}],
		Frame = {'@id'-'http://example.org/alice', '@explicit'- @true, 'http://schema.org/name'-[{}]},
		frame(Document, Frame, Framed).

	test(json_ld_frame_invalid_explicit_prolog_true, error(domain_error(json_ld_framing_boolean, _))) :-
		Document = [{'@id'-'http://example.org/alice'}],
		Frame = {'@id'-'http://example.org/alice', '@explicit'-true},
		frame(Document, Frame, _).

	% @requireAll true requires all framed properties to be present
	test(json_ld_frame_require_all, deterministic(Framed == [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}], 'http://schema.org/age'-[{'@value'-30}]}])) :-
		Document = [{'@id'-'http://example.org/alice', 'http://schema.org/name'-[{'@value'-'Alice'}], 'http://schema.org/age'-[{'@value'-30}]}, {'@id'-'http://example.org/bob', 'http://schema.org/name'-[{'@value'-'Bob'}]}],
		Frame = {'@requireAll'- @true, 'http://schema.org/name'-[{}], 'http://schema.org/age'-[{}]},
		frame(Document, Frame, Framed).

	% @default supplies missing framed properties unless omitted
	test(json_ld_frame_default, deterministic(Framed == [{'@id'-'http://example.org/alice', 'http://schema.org/nick'-['unknown']}])) :-
		Document = [{'@id'-'http://example.org/alice'}],
		Frame = {'@id'-'http://example.org/alice', '@explicit'- @true, 'http://schema.org/nick'-[{'@default'-'unknown'}]},
		frame(Document, Frame, Framed).

	% @omitDefault true suppresses missing default values
	test(json_ld_frame_omit_default, deterministic(Framed == [{'@id'-'http://example.org/alice'}])) :-
		Document = [{'@id'-'http://example.org/alice'}],
		Frame = {'@id'-'http://example.org/alice', '@explicit'- @true, '@omitDefault'- @true, 'http://schema.org/nick'-[{'@default'-'unknown'}]},
		frame(Document, Frame, Framed).

	% @reverse frames nodes that reference the matched node
	test(json_ld_frame_reverse, deterministic(Framed == [{'@id'-'http://example.org/alice', '@reverse'-{'http://schema.org/knows'-[{'@id'-'http://example.org/bob', 'http://schema.org/knows'-[{'@id'-'http://example.org/alice'}]}]}}])) :-
		Document = [{'@id'-'http://example.org/alice'}, {'@id'-'http://example.org/bob', 'http://schema.org/knows'-[{'@id'-'http://example.org/alice'}]}],
		Frame = {'@id'-'http://example.org/alice', '@reverse'-{'http://schema.org/knows'-[{}]}},
		frame(Document, Frame, Framed).

	% @list frames node references inside list objects
	test(json_ld_frame_list, deterministic(Framed == [{'@id'-'http://example.org/list', 'http://schema.org/items'-[{'@list'-[{'@id'-'http://example.org/item', 'http://schema.org/name'-[{'@value'-'Item'}]}]}]}])) :-
		Document = [{'@id'-'http://example.org/list', 'http://schema.org/items'-[{'@list'-[{'@id'-'http://example.org/item'}]}]}, {'@id'-'http://example.org/item', 'http://schema.org/name'-[{'@value'-'Item'}]}],
		Frame = {'@id'-'http://example.org/list', 'http://schema.org/items'-[{'@list'-[{}]}]},
		frame(Document, Frame, Framed).

	% Recursive embedding stops at cycles and keeps the repeated node as a reference
	test(json_ld_frame_cycle_prevention, deterministic(Framed == [{'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'@id'-'http://example.org/b', 'http://schema.org/knows'-[{'@id'-'http://example.org/a'}]}]}])) :-
		Document = [{'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'@id'-'http://example.org/b'}]}, {'@id'-'http://example.org/b', 'http://schema.org/knows'-[{'@id'-'http://example.org/a'}]}],
		Frame = {'@id'-'http://example.org/a', 'http://schema.org/knows'-[{'http://schema.org/knows'-[{}]}]},
		frame(Document, Frame, Framed).

:- end_object.
