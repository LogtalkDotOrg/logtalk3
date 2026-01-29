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
		date is 2026-01-29,
		comment is 'Tests for the json_schema library.'
	]).

	:- uses(json_schema, [
		parse/2, validate/2, validate/3
	]).

	:- uses(json, [
		parse/2 as json_parse/2
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	cover(json_schema(_,_,_)).
	cover(json_schema(_)).
	cover(json_schema).

	% =============== Schema Parsing Tests ===============

	test(parse_simple_string_schema, true) :-
		^^file_path('test_files/schemas/simple_string.json', Path),
		parse(file(Path), _Schema).

	test(parse_simple_number_schema, true) :-
		^^file_path('test_files/schemas/simple_number.json', Path),
		parse(file(Path), _Schema).

	test(parse_object_schema, true) :-
		^^file_path('test_files/schemas/object_with_properties.json', Path),
		parse(file(Path), _Schema).

	test(parse_boolean_true_schema, true(Schema == @true)) :-
		^^file_path('test_files/schemas/boolean_schema_true.json', Path),
		parse(file(Path), Schema).

	test(parse_boolean_false_schema, true(Schema == @false)) :-
		^^file_path('test_files/schemas/boolean_schema_false.json', Path),
		parse(file(Path), Schema).

	test(parse_schema_from_atom, true) :-
		parse(atom('{"type": "string"}'), _Schema).

	test(parse_schema_from_codes, true) :-
		atom_codes('{"type": "integer"}', Codes),
		parse(codes(Codes), _Schema).

	% =============== Type Validation Tests ===============

	test(validate_string_type_valid, true) :-
		parse(atom('{"type": "string"}'), Schema),
		validate(Schema, hello).

	test(validate_string_type_invalid, false) :-
		parse(atom('{"type": "string"}'), Schema),
		validate(Schema, 42).

	test(validate_number_type_valid_integer, true) :-
		parse(atom('{"type": "number"}'), Schema),
		validate(Schema, 42).

	test(validate_number_type_valid_float, true) :-
		parse(atom('{"type": "number"}'), Schema),
		validate(Schema, 3.14).

	test(validate_integer_type_valid, true) :-
		parse(atom('{"type": "integer"}'), Schema),
		validate(Schema, 42).

	test(validate_integer_type_invalid_float, false) :-
		parse(atom('{"type": "integer"}'), Schema),
		validate(Schema, 3.14).

	test(validate_boolean_type_true, true) :-
		parse(atom('{"type": "boolean"}'), Schema),
		validate(Schema, @true).

	test(validate_boolean_type_false, true) :-
		parse(atom('{"type": "boolean"}'), Schema),
		validate(Schema, @false).

	test(validate_null_type, true) :-
		parse(atom('{"type": "null"}'), Schema),
		validate(Schema, @null).

	test(validate_array_type, true) :-
		parse(atom('{"type": "array"}'), Schema),
		validate(Schema, [1, 2, 3]).

	test(validate_object_type_curly, true) :-
		parse(atom('{"type": "object"}'), Schema),
		validate(Schema, {a-1, b-2}).

	test(validate_object_type_empty, true) :-
		parse(atom('{"type": "object"}'), Schema),
		validate(Schema, {}).

	test(validate_multiple_types, true) :-
		parse(atom('{"type": ["string", "number"]}'), Schema),
		validate(Schema, hello),
		validate(Schema, 42).

	% =============== Boolean Schema Tests ===============

	test(validate_true_schema_accepts_all, true) :-
		validate(@true, hello),
		validate(@true, 42),
		validate(@true, [1, 2, 3]),
		validate(@true, {a-1}).

	test(validate_false_schema_rejects_all, false) :-
		validate(@false, hello).

	test(validate_empty_schema_accepts_all, true) :-
		validate({}, anything).

	% =============== Enum Validation Tests ===============

	test(validate_enum_valid, true) :-
		parse(atom('{"enum": ["red", "green", "blue"]}'), Schema),
		validate(Schema, red).

	test(validate_enum_invalid, false) :-
		parse(atom('{"enum": ["red", "green", "blue"]}'), Schema),
		validate(Schema, yellow).

	% =============== Const Validation Tests ===============

	test(validate_const_valid, true) :-
		parse(atom('{"const": "fixed"}'), Schema),
		validate(Schema, fixed).

	test(validate_const_invalid, false) :-
		parse(atom('{"const": "fixed"}'), Schema),
		validate(Schema, other).

	% =============== String Constraint Tests ===============

	test(validate_string_minlength_valid, true) :-
		parse(atom('{"type": "string", "minLength": 3}'), Schema),
		validate(Schema, hello).

	test(validate_string_minlength_invalid, false) :-
		parse(atom('{"type": "string", "minLength": 3}'), Schema),
		validate(Schema, hi).

	test(validate_string_maxlength_valid, true) :-
		parse(atom('{"type": "string", "maxLength": 5}'), Schema),
		validate(Schema, hello).

	test(validate_string_maxlength_invalid, false) :-
		parse(atom('{"type": "string", "maxLength": 5}'), Schema),
		validate(Schema, 'hello world').

	test(validate_string_length_range, true) :-
		parse(atom('{"type": "string", "minLength": 2, "maxLength": 10}'), Schema),
		validate(Schema, hello).

	% =============== Numeric Constraint Tests ===============

	test(validate_number_minimum_valid, true) :-
		parse(atom('{"type": "number", "minimum": 0}'), Schema),
		validate(Schema, 5).

	test(validate_number_minimum_invalid, false) :-
		parse(atom('{"type": "number", "minimum": 0}'), Schema),
		validate(Schema, -1).

	test(validate_number_maximum_valid, true) :-
		parse(atom('{"type": "number", "maximum": 100}'), Schema),
		validate(Schema, 50).

	test(validate_number_maximum_invalid, false) :-
		parse(atom('{"type": "number", "maximum": 100}'), Schema),
		validate(Schema, 150).

	test(validate_number_exclusive_minimum_valid, true) :-
		parse(atom('{"type": "number", "exclusiveMinimum": 0}'), Schema),
		validate(Schema, 1).

	test(validate_number_exclusive_minimum_invalid, false) :-
		parse(atom('{"type": "number", "exclusiveMinimum": 0}'), Schema),
		validate(Schema, 0).

	test(validate_number_exclusive_maximum_valid, true) :-
		parse(atom('{"type": "number", "exclusiveMaximum": 100}'), Schema),
		validate(Schema, 99).

	test(validate_number_exclusive_maximum_invalid, false) :-
		parse(atom('{"type": "number", "exclusiveMaximum": 100}'), Schema),
		validate(Schema, 100).

	test(validate_number_multiple_of_valid, true) :-
		parse(atom('{"type": "integer", "multipleOf": 5}'), Schema),
		validate(Schema, 15).

	test(validate_number_multiple_of_invalid, false) :-
		parse(atom('{"type": "integer", "multipleOf": 5}'), Schema),
		validate(Schema, 17).

	% =============== Array Constraint Tests ===============

	test(validate_array_items_valid, true) :-
		parse(atom('{"type": "array", "items": {"type": "integer"}}'), Schema),
		validate(Schema, [1, 2, 3]).

	test(validate_array_items_invalid, false) :-
		parse(atom('{"type": "array", "items": {"type": "integer"}}'), Schema),
		validate(Schema, [1, hello, 3]).

	test(validate_array_min_items_valid, true) :-
		parse(atom('{"type": "array", "minItems": 2}'), Schema),
		validate(Schema, [1, 2, 3]).

	test(validate_array_min_items_invalid, false) :-
		parse(atom('{"type": "array", "minItems": 2}'), Schema),
		validate(Schema, [1]).

	test(validate_array_max_items_valid, true) :-
		parse(atom('{"type": "array", "maxItems": 5}'), Schema),
		validate(Schema, [1, 2, 3]).

	test(validate_array_max_items_invalid, false) :-
		parse(atom('{"type": "array", "maxItems": 2}'), Schema),
		validate(Schema, [1, 2, 3]).

	test(validate_array_unique_items_valid, true) :-
		parse(atom('{"type": "array", "uniqueItems": true}'), Schema),
		validate(Schema, [1, 2, 3]).

	test(validate_array_unique_items_invalid, false) :-
		parse(atom('{"type": "array", "uniqueItems": true}'), Schema),
		validate(Schema, [1, 2, 2]).

	test(validate_array_contains_valid, true) :-
		parse(atom('{"type": "array", "contains": {"type": "string"}}'), Schema),
		validate(Schema, [1, hello, 3]).

	test(validate_array_contains_invalid, false) :-
		parse(atom('{"type": "array", "contains": {"type": "string"}}'), Schema),
		validate(Schema, [1, 2, 3]).

	% =============== Object Constraint Tests ===============

	test(validate_object_properties_valid, true) :-
		parse(atom('{"type": "object", "properties": {"name": {"type": "string"}, "age": {"type": "integer"}}}'), Schema),
		validate(Schema, {name-john, age-30}).

	test(validate_object_properties_invalid, false) :-
		parse(atom('{"type": "object", "properties": {"name": {"type": "string"}, "age": {"type": "integer"}}}'), Schema),
		validate(Schema, {name-john, age-thirty}).

	test(validate_object_required_valid, true) :-
		parse(atom('{"type": "object", "required": ["name"]}'), Schema),
		validate(Schema, {name-john, age-30}).

	test(validate_object_required_invalid, false) :-
		parse(atom('{"type": "object", "required": ["name", "email"]}'), Schema),
		validate(Schema, {name-john}).

	test(validate_object_min_properties_valid, true) :-
		parse(atom('{"type": "object", "minProperties": 2}'), Schema),
		validate(Schema, {a-1, b-2, c-3}).

	test(validate_object_min_properties_invalid, false) :-
		parse(atom('{"type": "object", "minProperties": 3}'), Schema),
		validate(Schema, {a-1, b-2}).

	test(validate_object_max_properties_valid, true) :-
		parse(atom('{"type": "object", "maxProperties": 3}'), Schema),
		validate(Schema, {a-1, b-2}).

	test(validate_object_max_properties_invalid, false) :-
		parse(atom('{"type": "object", "maxProperties": 2}'), Schema),
		validate(Schema, {a-1, b-2, c-3}).

	% =============== Composition Tests ===============

	test(validate_allof_valid, true) :-
		parse(atom('{"allOf": [{"type": "object"}, {"required": ["name"]}]}'), Schema),
		validate(Schema, {name-john}).

	test(validate_allof_invalid, false) :-
		parse(atom('{"allOf": [{"type": "object"}, {"required": ["name"]}]}'), Schema),
		validate(Schema, {age-30}).

	test(validate_anyof_valid_first, true) :-
		parse(atom('{"anyOf": [{"type": "string"}, {"type": "number"}]}'), Schema),
		validate(Schema, hello).

	test(validate_anyof_valid_second, true) :-
		parse(atom('{"anyOf": [{"type": "string"}, {"type": "number"}]}'), Schema),
		validate(Schema, 42).

	test(validate_anyof_invalid, false) :-
		parse(atom('{"anyOf": [{"type": "string"}, {"type": "number"}]}'), Schema),
		validate(Schema, @true).

	test(validate_oneof_valid, true) :-
		parse(atom('{"oneOf": [{"type": "string"}, {"type": "number"}]}'), Schema),
		validate(Schema, hello).

	test(validate_oneof_invalid_none, false) :-
		parse(atom('{"oneOf": [{"type": "string"}, {"type": "number"}]}'), Schema),
		validate(Schema, @true).

	test(validate_not_valid, true) :-
		parse(atom('{"not": {"type": "string"}}'), Schema),
		validate(Schema, 42).

	test(validate_not_invalid, false) :-
		parse(atom('{"not": {"type": "string"}}'), Schema),
		validate(Schema, hello).

	% =============== Conditional Validation Tests ===============

	% if/then: if validates, then is applied
	test(validate_if_then_applies_then_when_if_matches, true) :-
		% If type is integer, then must be >= 0
		parse(atom('{"if": {"type": "integer"}, "then": {"minimum": 0}}'), Schema),
		validate(Schema, 5).

	test(validate_if_then_fails_when_if_matches_but_then_fails, false) :-
		% If type is integer, then must be >= 0, but -5 fails
		parse(atom('{"if": {"type": "integer"}, "then": {"minimum": 0}}'), Schema),
		validate(Schema, -5).

	test(validate_if_then_passes_when_if_not_matches, true) :-
		% If type is integer (doesn't match string), then is not applied
		parse(atom('{"if": {"type": "integer"}, "then": {"minimum": 0}}'), Schema),
		validate(Schema, hello).

	% if/else: if doesn't validate, else is applied
	test(validate_if_else_applies_else_when_if_not_matches, true) :-
		% If type is integer (doesn't match), else requires minLength 3
		parse(atom('{"if": {"type": "integer"}, "else": {"minLength": 3}}'), Schema),
		validate(Schema, hello).

	test(validate_if_else_fails_when_if_not_matches_and_else_fails, false) :-
		% If type is integer (doesn't match), else requires minLength 5
		parse(atom('{"if": {"type": "integer"}, "else": {"minLength": 5}}'), Schema),
		validate(Schema, hi).

	test(validate_if_else_passes_when_if_matches, true) :-
		% If type is integer (matches), else is not applied
		parse(atom('{"if": {"type": "integer"}, "else": {"minLength": 100}}'), Schema),
		validate(Schema, 42).

	% if/then/else: full conditional
	test(validate_if_then_else_applies_then_when_if_matches, true) :-
		% If >= 10, then must be <= 100; else must be >= 0
		parse(atom('{"if": {"minimum": 10}, "then": {"maximum": 100}, "else": {"minimum": 0}}'), Schema),
		validate(Schema, 50).

	test(validate_if_then_else_applies_else_when_if_not_matches, true) :-
		% If >= 10 (doesn't match 5), else must be >= 0
		parse(atom('{"if": {"minimum": 10}, "then": {"maximum": 100}, "else": {"minimum": 0}}'), Schema),
		validate(Schema, 5).

	test(validate_if_then_else_fails_when_then_fails, false) :-
		% If >= 10 (matches 150), then must be <= 100 (fails)
		parse(atom('{"if": {"minimum": 10}, "then": {"maximum": 100}, "else": {"minimum": 0}}'), Schema),
		validate(Schema, 150).

	test(validate_if_then_else_fails_when_else_fails, false) :-
		% If >= 10 (doesn't match -5), else must be >= 0 (fails)
		parse(atom('{"if": {"minimum": 10}, "then": {"maximum": 100}, "else": {"minimum": 0}}'), Schema),
		validate(Schema, -5).

	% Conditional with object properties
	test(validate_if_then_else_with_objects, true) :-
		% If country is USA, then requires zip code
		parse(atom('{"if": {"properties": {"country": {"const": "USA"}}}, "then": {"required": ["zipCode"]}, "else": {"required": ["postalCode"]}}'), Schema),
		validate(Schema, {country-'USA', zipCode-'12345'}).

	test(validate_if_then_else_with_objects_else_branch, true) :-
		% If country is not USA, requires postalCode
		parse(atom('{"if": {"properties": {"country": {"const": "USA"}}}, "then": {"required": ["zipCode"]}, "else": {"required": ["postalCode"]}}'), Schema),
		validate(Schema, {country-'Canada', postalCode-'A1B 2C3'}).

	% =============== Schema Reference Tests ===============

	% $ref with $defs
	test(validate_ref_defs_valid, true) :-
		parse(atom('{"$defs": {"posInt": {"type": "integer", "minimum": 0}}, "properties": {"count": {"$ref": "#/$defs/posInt"}}}'), Schema),
		validate(Schema, {count-42}).

	test(validate_ref_defs_invalid, false) :-
		parse(atom('{"$defs": {"posInt": {"type": "integer", "minimum": 0}}, "properties": {"count": {"$ref": "#/$defs/posInt"}}}'), Schema),
		validate(Schema, {count-(-5)}).

	test(validate_ref_defs_type_mismatch, false) :-
		parse(atom('{"$defs": {"posInt": {"type": "integer", "minimum": 0}}, "properties": {"count": {"$ref": "#/$defs/posInt"}}}'), Schema),
		validate(Schema, {count-hello}).

	% $ref with definitions (draft-07 style)
	test(validate_ref_definitions_valid, true) :-
		parse(atom('{"definitions": {"stringType": {"type": "string"}}, "properties": {"name": {"$ref": "#/definitions/stringType"}}}'), Schema),
		validate(Schema, {name-john}).

	test(validate_ref_definitions_invalid, false) :-
		parse(atom('{"definitions": {"stringType": {"type": "string"}}, "properties": {"name": {"$ref": "#/definitions/stringType"}}}'), Schema),
		validate(Schema, {name-123}).

	% Multiple $refs in same schema
	test(validate_ref_multiple_refs, true) :-
		parse(atom('{"$defs": {"str": {"type": "string"}, "num": {"type": "number"}}, "properties": {"name": {"$ref": "#/$defs/str"}, "age": {"$ref": "#/$defs/num"}}}'), Schema),
		validate(Schema, {name-john, age-30}).

	% $ref with complex schema
	test(validate_ref_complex_schema, true) :-
		parse(atom('{"$defs": {"address": {"type": "object", "required": ["street", "city"]}}, "properties": {"home": {"$ref": "#/$defs/address"}}}'), Schema),
		validate(Schema, {home-{street-'Main St', city-'Boston'}}).

	test(validate_ref_complex_schema_missing_required, false) :-
		parse(atom('{"$defs": {"address": {"type": "object", "required": ["street", "city"]}}, "properties": {"home": {"$ref": "#/$defs/address"}}}'), Schema),
		validate(Schema, {home-{street-'Main St'}}).

	% Unresolved $ref returns error
	test(validate_ref_unresolved, true(Errors \== [])) :-
		parse(atom('{"properties": {"x": {"$ref": "#/$defs/nonexistent"}}}'), Schema),
		validate(Schema, {x-1}, Errors).

	% =============== Format Validation Tests ===============

	% Email format
	test(validate_format_email_valid, true) :-
		parse(atom('{"format": "email"}'), Schema),
		validate(Schema, 'user@example.com').

	test(validate_format_email_invalid_no_at, false) :-
		parse(atom('{"format": "email"}'), Schema),
		validate(Schema, 'userexample.com').

	test(validate_format_email_invalid_no_local, false) :-
		parse(atom('{"format": "email"}'), Schema),
		validate(Schema, '@example.com').

	test(validate_format_email_invalid_no_domain, false) :-
		parse(atom('{"format": "email"}'), Schema),
		validate(Schema, 'user@').

	% Date format (YYYY-MM-DD)
	test(validate_format_date_valid, true) :-
		parse(atom('{"format": "date"}'), Schema),
		validate(Schema, '2024-01-15').

	test(validate_format_date_valid_leap_year, true) :-
		parse(atom('{"format": "date"}'), Schema),
		validate(Schema, '2024-02-29').

	test(validate_format_date_invalid_format, false) :-
		parse(atom('{"format": "date"}'), Schema),
		validate(Schema, '01-15-2024').

	test(validate_format_date_invalid_month, false) :-
		parse(atom('{"format": "date"}'), Schema),
		validate(Schema, '2024-13-15').

	test(validate_format_date_invalid_day, false) :-
		parse(atom('{"format": "date"}'), Schema),
		validate(Schema, '2024-02-30').

	% Time format (HH:MM:SS)
	test(validate_format_time_valid, true) :-
		parse(atom('{"format": "time"}'), Schema),
		validate(Schema, '14:30:00').

	test(validate_format_time_valid_with_timezone, true) :-
		parse(atom('{"format": "time"}'), Schema),
		validate(Schema, '14:30:00Z').

	test(validate_format_time_valid_with_offset, true) :-
		parse(atom('{"format": "time"}'), Schema),
		validate(Schema, '14:30:00+05:30').

	test(validate_format_time_invalid_hour, false) :-
		parse(atom('{"format": "time"}'), Schema),
		validate(Schema, '25:30:00').

	test(validate_format_time_invalid_minute, false) :-
		parse(atom('{"format": "time"}'), Schema),
		validate(Schema, '14:60:00').

	% Date-time format
	test(validate_format_datetime_valid, true) :-
		parse(atom('{"format": "date-time"}'), Schema),
		validate(Schema, '2024-01-15T14:30:00').

	test(validate_format_datetime_valid_with_tz, true) :-
		parse(atom('{"format": "date-time"}'), Schema),
		validate(Schema, '2024-01-15T14:30:00Z').

	test(validate_format_datetime_invalid, false) :-
		parse(atom('{"format": "date-time"}'), Schema),
		validate(Schema, '2024-01-15 25:30:00').

	% URI format
	test(validate_format_uri_valid_http, true) :-
		parse(atom('{"format": "uri"}'), Schema),
		validate(Schema, 'http://example.com').

	test(validate_format_uri_valid_https, true) :-
		parse(atom('{"format": "uri"}'), Schema),
		validate(Schema, 'https://example.com/path').

	test(validate_format_uri_valid_mailto, true) :-
		parse(atom('{"format": "uri"}'), Schema),
		validate(Schema, 'mailto:user@example.com').

	test(validate_format_uri_invalid_no_scheme, false) :-
		parse(atom('{"format": "uri"}'), Schema),
		validate(Schema, 'example.com').

	% IPv4 format
	test(validate_format_ipv4_valid, true) :-
		parse(atom('{"format": "ipv4"}'), Schema),
		validate(Schema, '192.168.1.1').

	test(validate_format_ipv4_valid_zeros, true) :-
		parse(atom('{"format": "ipv4"}'), Schema),
		validate(Schema, '0.0.0.0').

	test(validate_format_ipv4_invalid_octet_range, false) :-
		parse(atom('{"format": "ipv4"}'), Schema),
		validate(Schema, '256.168.1.1').

	test(validate_format_ipv4_invalid_too_few_octets, false) :-
		parse(atom('{"format": "ipv4"}'), Schema),
		validate(Schema, '192.168.1').

	test(validate_format_ipv4_invalid_leading_zero, false) :-
		parse(atom('{"format": "ipv4"}'), Schema),
		validate(Schema, '192.168.01.1').

	% IPv6 format
	test(validate_format_ipv6_valid_full, true) :-
		parse(atom('{"format": "ipv6"}'), Schema),
		validate(Schema, '2001:0db8:85a3:0000:0000:8a2e:0370:7334').

	test(validate_format_ipv6_valid_compressed, true) :-
		parse(atom('{"format": "ipv6"}'), Schema),
		validate(Schema, '2001:db8::1').

	test(validate_format_ipv6_valid_loopback, true) :-
		parse(atom('{"format": "ipv6"}'), Schema),
		validate(Schema, '::1').

	test(validate_format_ipv6_invalid_too_many_groups, false) :-
		parse(atom('{"format": "ipv6"}'), Schema),
		validate(Schema, '2001:0db8:85a3:0000:0000:8a2e:0370:7334:1234').

	% UUID format
	test(validate_format_uuid_valid, true) :-
		parse(atom('{"format": "uuid"}'), Schema),
		validate(Schema, '550e8400-e29b-41d4-a716-446655440000').

	test(validate_format_uuid_invalid_wrong_length, false) :-
		parse(atom('{"format": "uuid"}'), Schema),
		validate(Schema, '550e8400-e29b-41d4-a716-44665544000').

	test(validate_format_uuid_invalid_wrong_pattern, false) :-
		parse(atom('{"format": "uuid"}'), Schema),
		validate(Schema, '550e8400e29b41d4a716446655440000').

	% Format on non-string passes (per JSON Schema spec)
	test(validate_format_nonstring_passes, true) :-
		parse(atom('{"format": "email"}'), Schema),
		validate(Schema, 42).

	% Unknown format passes
	test(validate_format_unknown_passes, true) :-
		parse(atom('{"format": "unknown-format"}'), Schema),
		validate(Schema, hello).

	% =============== Error Reporting Tests ===============

	test(validate_with_errors_returns_list, true(Errors == [])) :-
		parse(atom('{"type": "string"}'), Schema),
		validate(Schema, hello, Errors).

	test(validate_with_errors_type_mismatch, true(Errors \== [])) :-
		parse(atom('{"type": "string"}'), Schema),
		validate(Schema, 42, Errors).

	test(validate_with_errors_required_missing, true) :-
		parse(atom('{"type": "object", "required": ["name"]}'), Schema),
		validate(Schema, {age-30}, Errors),
		Errors = [error(_, missing_required(name))].

	% =============== Real-world Schema Tests ===============

	test(validate_person_schema_valid, true) :-
		^^file_path('test_files/schemas/person.json', SchemaPath),
		^^file_path('test_files/data/valid_person.json', DataPath),
		parse(file(SchemaPath), Schema),
		json_parse(file(DataPath), JSON),
		validate(Schema, JSON).

	test(validate_person_schema_missing_required, false) :-
		^^file_path('test_files/schemas/person.json', SchemaPath),
		^^file_path('test_files/data/invalid_person_missing_required.json', DataPath),
		parse(file(SchemaPath), Schema),
		json_parse(file(DataPath), JSON),
		validate(Schema, JSON).

	test(validate_address_schema_valid, true) :-
		^^file_path('test_files/schemas/address.json', SchemaPath),
		^^file_path('test_files/data/valid_address.json', DataPath),
		parse(file(SchemaPath), Schema),
		json_parse(file(DataPath), JSON),
		validate(Schema, JSON).

	test(validate_product_schema_valid, true) :-
		^^file_path('test_files/schemas/product.json', SchemaPath),
		^^file_path('test_files/data/valid_product.json', DataPath),
		parse(file(SchemaPath), Schema),
		json_parse(file(DataPath), JSON),
		validate(Schema, JSON).

:- end_object.

