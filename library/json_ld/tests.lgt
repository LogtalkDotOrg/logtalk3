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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-07,
		comment is 'Tests for the "json_ld" library.'
	]).

	:- uses(json_ld, [
		parse/2, generate/2, expand/2, compact/3, flatten/2
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

	% =============== Parsing Tests ===============

	test(json_ld_parse_from_file, true) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		parse(file(Path), Term),
		assertion(ground(Term)).

	test(json_ld_parse_from_atom, true(Term == {'@id'-'http://example.org/1'})) :-
		parse(atom('{"@id":"http://example.org/1"}'), Term).

	test(json_ld_parse_from_codes, true) :-
		atom_codes('{"@type":"Person"}', Codes),
		parse(codes(Codes), Term),
		assertion(Term == {'@type'-'Person'}).

	test(json_ld_parse_from_chars, true) :-
		atom_chars('{"name":"test"}', Chars),
		parse(chars(Chars), Term),
		assertion(Term == {name-test}).

	test(json_ld_parse_simple_person, true) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		parse(file(Path), Term),
		assertion(Term = {'@context'-_, name-'Manu Sporny', homepage-'http://manu.sporny.org/', image-'http://manu.sporny.org/images/manu.png'}).

	test(json_ld_parse_graph, true) :-
		^^file_path('test_files/graph.jsonld', Path),
		parse(file(Path), Term),
		assertion(Term = {'@context'-_, '@graph'-_}).

	test(json_ld_parse_expanded, true) :-
		^^file_path('test_files/expanded.jsonld', Path),
		parse(file(Path), Term),
		assertion(is_list(Term)).

	test(json_ld_parse_empty_context, true) :-
		^^file_path('test_files/empty_context.jsonld', Path),
		parse(file(Path), _Term).

	% =============== Generation Tests ===============

	test(json_ld_generate_to_atom, true) :-
		generate(atom(Atom), {'@id'-'http://example.org/1'}),
		assertion(atom(Atom)).

	test(json_ld_generate_to_codes, true) :-
		generate(codes(Codes), {'@id'-'http://example.org/1'}),
		assertion(is_list(Codes)).

	test(json_ld_generate_to_chars, true) :-
		generate(chars(Chars), {'@id'-'http://example.org/1'}),
		assertion(is_list(Chars)).

	test(json_ld_roundtrip_atom, true(Term == {'@id'-'http://example.org/1', name-test})) :-
		generate(atom(Atom), {'@id'-'http://example.org/1', name-test}),
		parse(atom(Atom), Term).

	% =============== Expansion Tests: Core Features ===============

	test(json_ld_expand_simple_context, true) :-
		parse(atom('{"@context":{"name":"http://schema.org/name"},"name":"Manu"}'), Doc),
		expand(Doc, Expanded),
		assertion(is_list(Expanded)),
		Expanded = [Node],
		assertion(Node = {'http://schema.org/name'-_}).

	test(json_ld_expand_id, true) :-
		parse(atom('{"@context":{"name":"http://schema.org/name"},"@id":"http://example.org/1","name":"Test"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/1', 'http://schema.org/name'-_}).

	test(json_ld_expand_type_single, true) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/"},"@type":"Person"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-['http://schema.org/Person']}).

	test(json_ld_expand_type_multiple, true) :-
		^^file_path('test_files/multiple_types.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', '@type'-['http://schema.org/Person', 'http://schema.org/Author'], _}).

	test(json_ld_expand_vocab, true) :-
		^^file_path('test_files/context_vocab.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-['http://schema.org/Person'], 'http://schema.org/name'-_, 'http://schema.org/jobTitle'-_}).

	test(json_ld_expand_base, true) :-
		^^file_path('test_files/context_base.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/person/1', 'http://schema.org/name'-_}).

	test(json_ld_expand_compact_iri, true) :-
		^^file_path('test_files/compact_iri.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://xmlns.com/foaf/0.1/name'-_, 'http://purl.org/dc/terms/title'-_}).

	% =============== Expansion Tests: Value Objects ===============

	test(json_ld_expand_typed_value, true) :-
		^^file_path('test_files/typed_value.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://purl.org/dc/terms/modified'-_}).

	test(json_ld_expand_language_tagged, true) :-
		^^file_path('test_files/language_tagged.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/name'-_, 'http://schema.org/description'-_}).

	test(json_ld_expand_default_language, true) :-
		^^file_path('test_files/default_language.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-_, 'http://schema.org/name'-_, 'http://schema.org/author'-_}).

	% =============== Expansion Tests: Graph and Structure ===============

	test(json_ld_expand_graph, true) :-
		^^file_path('test_files/graph.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@graph'-_}).

	test(json_ld_expand_reverse, true) :-
		^^file_path('test_files/reverse_property.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://schema.org/name'-_, '@reverse'-_}).

	test(json_ld_expand_list, true) :-
		^^file_path('test_files/list.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://xmlns.com/foaf/0.1/nick'-_}).

	test(json_ld_expand_set, true) :-
		^^file_path('test_files/set.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', _}).

	test(json_ld_expand_included, true) :-
		^^file_path('test_files/included.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/collection', '@type'-_, '@included'-_}).

	test(json_ld_expand_index, true) :-
		^^file_path('test_files/index.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', '@type'-_, 'http://schema.org/name'-_, '@index'-'person-index'}).

	test(json_ld_expand_blank_node, true) :-
		^^file_path('test_files/blank_node.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'_:b0', 'http://schema.org/name'-_, 'http://schema.org/knows'-_}).

	% =============== Expansion Tests: Multiple Contexts ===============

	test(json_ld_expand_multiple_contexts, true) :-
		^^file_path('test_files/multiple_contexts.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/name'-_, 'http://schema.org/url'-_}).

	% =============== Expansion Tests: Nested Objects ===============

	test(json_ld_expand_nested_objects, true) :-
		^^file_path('test_files/nested_objects.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-_, 'http://schema.org/name'-_, 'http://schema.org/knows'-_}).

	test(json_ld_expand_schema_org_event, true) :-
		^^file_path('test_files/schema_org_event.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@type'-_, 'http://schema.org/name'-_, 'http://schema.org/startDate'-_, 'http://schema.org/endDate'-_, 'http://schema.org/location'-_, 'http://schema.org/organizer'-_}).

	% =============== Expansion Tests: Type Coercion ===============

	test(json_ld_expand_type_coercion_id, true) :-
		^^file_path('test_files/type_coercion.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/name'-_, 'http://schema.org/url'-_}).

	test(json_ld_expand_expanded_term_def, true) :-
		^^file_path('test_files/expanded_term_definition.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'@id'-'http://example.org/people/1', 'http://schema.org/name'-_, 'http://schema.org/age'-_, 'http://schema.org/url'-_}).

	% =============== Expansion Tests: Scalars and Special Values ===============

	test(json_ld_expand_boolean_true, true) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"active":true}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/active'-_}).

	test(json_ld_expand_boolean_false, true) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"active":false}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/active'-_}).

	test(json_ld_expand_null_value, true) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"prop":null}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/prop'-_}).

	test(json_ld_expand_number_integer, true) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"count":42}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/count'-_}).

	test(json_ld_expand_number_float, true) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"value":3.14}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/value'-_}).

	test(json_ld_expand_array_values, true) :-
		parse(atom('{"@context":{"@vocab":"http://example.org/"},"tags":["a","b","c"]}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://example.org/tags'-_}).

	% =============== Expansion Tests: Already-Expanded Form ===============

	test(json_ld_expand_already_expanded_array, true) :-
		^^file_path('test_files/expanded.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% =============== Expansion Tests: Inline Atom Sources ===============

	test(json_ld_expand_empty_object, true(Expanded == [])) :-
		expand({}, Expanded).

	test(json_ld_expand_no_context, true) :-
		expand({'http://schema.org/name'-'Test'}, Expanded),
		assertion(is_list(Expanded)).

	% =============== Compaction Tests ===============

	test(json_ld_compact_simple, true) :-
		Context = {'@context'-{name-'http://schema.org/name'}},
		expand({'@context'-{name-'http://schema.org/name'}, name-'Manu'}, Expanded),
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	test(json_ld_compact_with_vocab, true) :-
		Context = {'@context'-{'@vocab'-'http://schema.org/'}},
		expand({'@context'-{'@vocab'-'http://schema.org/'}, '@type'-'Person', name-'Jane'}, Expanded),
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	test(json_ld_compact_empty_list, true(Compacted == [])) :-
		compact([], {'@context'-{}}, Compacted).

	% =============== Parametric Object Tests ===============

	test(json_ld_parse_curly_dash_atom, true) :-
		json_ld(curly, dash, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == {'@id'-'http://example.org/1'}).

	test(json_ld_parse_list_dash_atom, true) :-
		json_ld(list, dash, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == json(['@id'-'http://example.org/1'])).

	test(json_ld_parse_list_equal_atom, true) :-
		json_ld(list, equal, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == json(['@id'='http://example.org/1'])).

	test(json_ld_parse_list_colon_atom, true) :-
		json_ld(list, colon, atom)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == json([':'('@id','http://example.org/1')])).

	test(json_ld_parse_curly_dash_chars, true) :-
		json_ld(curly, dash, chars)::parse(atom('{"@id":"http://example.org/1"}'), Term),
		assertion(Term == {chars(['@',i,d])-chars([h,t,t,p,:,/,/,e,x,a,m,p,l,e,'.',o,r,g,/,'1'])}).

	test(json_ld_shorthand_object_1, true) :-
		json_ld(atom)::parse(atom('{"name":"test"}'), Term),
		assertion(Term == {name-test}).

	test(json_ld_shorthand_object_0, true) :-
		json_ld::parse(atom('{"name":"test"}'), Term),
		assertion(Term == {name-test}).

	% =============== Expansion with List Representation ===============

	test(json_ld_expand_list_representation, true) :-
		json_ld(list, dash, atom)::parse(atom('{"@context":{"name":"http://schema.org/name"},"name":"Test"}'), Doc),
		json_ld(list, dash, atom)::expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% =============== Roundtrip Tests ===============

	test(json_ld_roundtrip_file, true) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		parse(file(Path), Term),
		generate(atom(Atom), Term),
		assertion(atom(Atom)),
		parse(atom(Atom), Term2),
		assertion(ground(Term2)).

	test(json_ld_roundtrip_expand_compact, true) :-
		Doc = {'@context'-{name-'http://schema.org/name'}, name-'Manu'},
		Context = {'@context'-{name-'http://schema.org/name'}},
		expand(Doc, Expanded),
		compact(Expanded, Context, _Compacted).

	% =============== Direction Tests ===============

	test(json_ld_expand_direction, true) :-
		^^file_path('test_files/direction.jsonld', Path),
		parse(file(Path), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% =============== Additional Coverage Tests ===============

	% expand_document/3 clause 1: empty list
	test(json_ld_expand_empty_list, true(Expanded == [])) :-
		expand([], Expanded).

	% expand_document/3 clause 4: scalar value
	test(json_ld_expand_scalar_value, true(Expanded == [42])) :-
		expand(42, Expanded).

	% expand_document/3 clause 4: string value
	test(json_ld_expand_string_value, true(Expanded == [hello])) :-
		expand(hello, Expanded).

	% compact_document/3 clause 4: scalar value
	test(json_ld_compact_scalar_value, true(Compacted == 42)) :-
		compact(42, {'@context'-{}}, Compacted).

	% compact_value_array/3 clause 1: empty array
	test(json_ld_compact_value_empty_array, true) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}, items-[]}, Expanded),
		compact(Expanded, {'@context'-{'@vocab'-'http://example.org/'}}, Compacted),
		assertion(ground(Compacted)).

	% compact_value_array/3 clause 3: multi-element array
	test(json_ld_compact_value_multi_array, true) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}, items-[1, 2, 3]}, Expanded),
		compact(Expanded, {'@context'-{'@vocab'-'http://example.org/'}}, Compacted),
		assertion(ground(Compacted)).

	% process_context_entry/4: @direction handling
	test(json_ld_context_direction, true) :-
		parse(atom('{"@context":{"@direction":"rtl","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% process_context_entry/4: @version handling (ignored)
	test(json_ld_context_version, true) :-
		parse(atom('{"@context":{"@version":1.1,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% process_context_entry/4: @propagate handling (ignored)
	test(json_ld_context_propagate, true) :-
		parse(atom('{"@context":{"@propagate":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% process_context_entry/4: @protected handling (ignored)
	test(json_ld_context_protected, true) :-
		parse(atom('{"@context":{"@protected":true,"name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% process_context_entry/4: @import handling (ignored)
	test(json_ld_context_import, true) :-
		parse(atom('{"@context":{"@import":"http://example.org/context","name":"http://schema.org/name"},"name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% process_context_entry/4: term definition to null
	test(json_ld_context_term_null, true) :-
		parse(atom('{"@context":{"name":null,"title":"http://schema.org/title"},"title":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% remove_context_value/3: @vocab to null (tests context array processing)
	test(json_ld_context_vocab_null, true) :-
		parse(atom('{"@context":[{"@vocab":"http://schema.org/"},{"@vocab":null}],"http://schema.org/name":"test"}'), Doc),
		expand(Doc, Expanded),
		% The full IRI property should still expand normally
		assertion(is_list(Expanded)).

	% remove_context_value/3: @base to null
	test(json_ld_context_base_null, true) :-
		parse(atom('{"@context":[{"@base":"http://example.org/"},{"@base":null}],"@id":"foo"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% remove_context_value/3: @language to null
	test(json_ld_context_language_null, true) :-
		parse(atom('{"@context":[{"@language":"en"},{"@language":null}],"http://schema.org/name":"test"}'), Doc),
		expand(Doc, [Node]),
		assertion(ground(Node)).

	% make_value_object_id/2: @type: @id coercion
	test(json_ld_type_coercion_id, true) :-
		parse(atom('{"@context":{"homepage":{"@id":"http://schema.org/url","@type":"@id"}},"homepage":"http://example.org/"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/url'-[{'@id'-'http://example.org/'}]}).

	% make_value_object_id/2: @type: @vocab coercion
	test(json_ld_type_coercion_vocab, true) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/","status":{"@type":"@vocab"}},"status":"Active"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/status'-[{'@id'-'http://schema.org/Active'}]}).

	% make_typed_value_object/3: typed literal coercion
	test(json_ld_type_coercion_typed_literal, true) :-
		parse(atom('{"@context":{"age":{"@id":"http://schema.org/age","@type":"http://www.w3.org/2001/XMLSchema#integer"}},"age":"30"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/age'-[{'@value'-'30', '@type'-'http://www.w3.org/2001/XMLSchema#integer'}]}).

	% find_prefix_for_iri/4: prefix:suffix compaction
	test(json_ld_compact_prefix_iri, true) :-
		Expanded = [{'http://xmlns.com/foaf/0.1/name'-[{'@value'-'Alice'}]}],
		Context = {'@context'-{foaf-'http://xmlns.com/foaf/0.1/'}},
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	% pairs_to_object/2 clause 1: empty pairs
	test(json_ld_expand_object_no_properties, true) :-
		expand({'@context'-{'@vocab'-'http://example.org/'}}, Expanded),
		assertion(Expanded == []).

	% parse/2 clause 3: stream parsing
	test(json_ld_parse_from_stream, true) :-
		^^file_path('test_files/simple_person.jsonld', Path),
		open(Path, read, Stream),
		parse(stream(Stream), Term),
		close(Stream),
		assertion(ground(Term)).

	% generate/2 clause 2: file generation
	test(json_ld_generate_to_file, true) :-
		^^file_path('test_files/output.jsonld', Path),
		generate(file(Path), {'@id'-'http://example.org/1'}),
		parse(file(Path), Term),
		assertion(Term == {'@id'-'http://example.org/1'}).

	% generate/2 clause 3: stream generation
	test(json_ld_generate_to_stream, true) :-
		^^file_path('test_files/output2.jsonld', Path),
		open(Path, write, Stream),
		generate(stream(Stream), {'@id'-'http://example.org/1'}),
		close(Stream),
		parse(file(Path), Term),
		assertion(Term == {'@id'-'http://example.org/1'}).

	% pair_key_value/3 with equal representation
	test(json_ld_expand_equal_representation, true) :-
		json_ld(list, equal, atom)::expand(json(['@context'={'@vocab'='http://example.org/'}, name='Test']), Expanded),
		assertion(is_list(Expanded)).

	% pair_key_value/3 with colon representation
	test(json_ld_expand_colon_representation, true) :-
		json_ld(list, colon, atom)::expand(json([':'('@context',':'('@vocab','http://example.org/')), ':'(name,'Test')]), Expanded),
		assertion(is_list(Expanded)).

	% expand_pair/4 clause 13: @nest (ignored)
	test(json_ld_expand_nest, true) :-
		parse(atom('{"@context":{"@vocab":"http://schema.org/"},"@nest":{"name":"Test"}}'), Doc),
		expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% expand_value/4 clause 6: non-atom value fallback (chars/codes)
	test(json_ld_expand_chars_value, true) :-
		json_ld(curly, dash, chars)::parse(atom('{"@context":{"@vocab":"http://example.org/"},"name":"test"}'), Doc),
		json_ld(curly, dash, chars)::expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% expand_iri/4 clause 9: fallback for unresolvable IRI
	test(json_ld_expand_unresolvable_iri, true) :-
		parse(atom('{"@context":{},"unknownProp":"value"}'), Doc),
		expand(Doc, Expanded),
		% unknownProp should remain unexpanded and get filtered out
		assertion(is_list(Expanded)).

	% Keyword expansion coverage - ensure @json is tested
	test(json_ld_keyword_json, true) :-
		parse(atom('{"@context":{"data":{"@id":"http://example.org/data","@type":"@json"}},"data":{"key":"value"}}'), Doc),
		expand(Doc, Expanded),
		assertion(is_list(Expanded)).

	% Type coercion with list values - expand_list_elements/4 with type coercion
	test(json_ld_type_coercion_id_list, true) :-
		parse(atom('{"@context":{"links":{"@id":"http://schema.org/links","@type":"@id"}},"links":["http://example.org/a","http://example.org/b"]}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/links'-[{'@id'-'http://example.org/a'}, {'@id'-'http://example.org/b'}]}).

	% find_prefix_for_iri/4 with multiple prefixes - tests clause 3 (recursive)
	test(json_ld_compact_multiple_prefixes, true) :-
		Expanded = [{'http://purl.org/dc/terms/title'-[{'@value'-'Title'}], 'http://xmlns.com/foaf/0.1/name'-[{'@value'-'Name'}]}],
		Context = {'@context'-{dc-'http://purl.org/dc/terms/', foaf-'http://xmlns.com/foaf/0.1/'}},
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	% Compact with term definition in context - term def entries have compound keys that
	% are filtered by atom(Key) check, not separate is_term_def_key predicate
	test(json_ld_compact_with_term_def, true) :-
		Expanded = [{'http://schema.org/name'-[{'@value'-'Alice'}]}],
		Context = {'@context'-{name-{'@id'-'http://schema.org/name'}}},
		compact(Expanded, Context, Compacted),
		assertion(ground(Compacted)).

	% Compact IRI not matching term, uses prefix search
	test(json_ld_compact_skip_term_def_key, true) :-
		Expanded = [{'http://xmlns.com/foaf/0.1/homepage'-[{'@id'-'http://example.org/'}]}],
		% Context has both term def and prefix
		Context = {'@context'-{foaf-'http://xmlns.com/foaf/0.1/', knows-{'@id'-'http://xmlns.com/foaf/0.1/knows'}}},
		compact(Expanded, Context, Compacted),
		% Should compact to foaf:homepage using prefix (not matching knows term)
		assertion(ground(Compacted)).

	% Type coercion with @vocab in nested context
	test(json_ld_type_coercion_vocab_nested, true) :-
		parse(atom('{"@context":[{"@vocab":"http://schema.org/"},{"status":{"@type":"@vocab"}}],"status":"Active"}'), Doc),
		expand(Doc, [Node]),
		assertion(Node = {'http://schema.org/status'-[{'@id'-'http://schema.org/Active'}]}).

	% =============== Flatten Tests ===============

	% Error handling test for flatten
	test(json_ld_flatten_error_var_document, error(instantiation_error)) :-
		flatten(_, _).

	% Flatten a simple expanded node
	test(json_ld_flatten_simple_node, true) :-
		Expanded = [{'@id'-'http://example.org/john', 'http://schema.org/name'-[{'@value'-'John'}]}],
		flatten(Expanded, Flattened),
		assertion(Flattened = {'@id'-'http://example.org/john', 'http://schema.org/name'-[{'@value'-'John'}]}).

	% Flatten nested nodes - nested node should be extracted
	test(json_ld_flatten_nested_nodes, true) :-
		Expanded = [{'@id'-'http://example.org/john',
			'http://schema.org/knows'-[{'@id'-'http://example.org/jane', 'http://schema.org/name'-[{'@value'-'Jane'}]}]}],
		flatten(Expanded, Flattened),
		% Result should be an object with @graph containing two nodes
		Flattened = {'@graph'-Graph},
		assertion(is_list(Graph)),
		assertion(length(Graph, 2)).

	% Flatten generates blank node IDs for nodes without @id
	test(json_ld_flatten_blank_node_generation, true) :-
		Expanded = [{'http://schema.org/name'-[{'@value'-'Anonymous'}]}],
		flatten(Expanded, Flattened),
		% Result should have a generated blank node ID starting with _:b
		Flattened = {'@id'-Id, _},
		assertion(sub_atom(Id, 0, _, _, '_:b')).

	% Flatten an empty expanded document
	test(json_ld_flatten_empty, true) :-
		flatten([], Flattened),
		assertion(Flattened = {'@graph'-[]}).

	% Flatten multiple top-level nodes
	test(json_ld_flatten_multiple_nodes, true) :-
		Expanded = [{'@id'-'http://example.org/a'}, {'@id'-'http://example.org/b'}],
		flatten(Expanded, Flattened),
		Flattened = {'@graph'-Graph},
		assertion(length(Graph, 2)).

	% Flatten merges nodes with same @id
	test(json_ld_flatten_merge_same_id, true) :-
		% Two nodes with same @id but different properties
		Expanded = [{'@id'-'http://example.org/a', 'http://schema.org/name'-[{'@value'-'Name'}]},
			{'@id'-'http://example.org/a', 'http://schema.org/age'-[{'@value'-30}]}],
		flatten(Expanded, Flattened),
		% Result should merge into single node with both properties
		assertion(ground(Flattened)).

	% Flatten preserves @type
	test(json_ld_flatten_preserves_type, true) :-
		Expanded = [{'@id'-'http://example.org/john', '@type'-['http://schema.org/Person']}],
		flatten(Expanded, Flattened),
		Flattened = {'@id'-_, '@type'-Types},
		assertion(Types = ['http://schema.org/Person']).

	% Test flattening through parse and expand workflow
	test(json_ld_flatten_workflow, true) :-
		parse(atom('{"@context":{"name":"http://schema.org/name"},"@id":"http://example.org/test","name":"Test"}'), Doc),
		expand(Doc, Expanded),
		flatten(Expanded, Flattened),
		assertion(ground(Flattened)).

:- end_object.
