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


:- object(open_api,
	imports(http_json_term_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'OpenAPI 3.1.0 document derivation, parsing, generation, and validation built on top of the ``json`` and ``json_schema`` libraries.',
		see_also is [json, json_schema, application_protocol, open_api_provider_protocol]
	]).

	:- uses(json, [
		parse/2 as json_parse/2, generate/2 as json_generate/2
	]).

	:- uses(json_schema, [
		validate/3 as json_schema_validate/3
	]).

	:- uses(http_core, [
		parse_request/2 as http_parse_request/2, parse_response/2 as http_parse_response/2,
		is_request/1 as http_is_request/1, is_response/1 as http_is_response/1, method/2 as http_method/2,
		target/2 as http_target/2, headers/2 as http_headers/2, body/2 as http_body/2,
		property/2 as http_property/2, status/2 as http_status/2
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	:- public(document/2).
	:- mode(document(+object_identifier, -compound), one).
	:- info(document/2, [
		comment is 'Derives an OpenAPI 3.1.0 document in the repository-standard JSON term representation from an object implementing the ``open_api_provider_protocol`` protocol. When the provider also implements the ``application_protocol`` protocol, missing ``description`` and ``license`` info fields may be backfilled from application metadata and ``homepage/1`` may be mapped to the top-level ``externalDocs`` object. Top-level security defaults declared by ``security/1`` are emitted under the root ``security`` field. Reusable security schemes declared by ``security_scheme/2`` are emitted under ``components.securitySchemes`` and referenced scheme names and OAuth flow scopes are validated against those declarations when locally available.',
		argnames is ['Provider', 'Document']
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses an OpenAPI document from the given source (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) into the JSON term representation used by the ``json`` library.',
		argnames is ['Source', 'Document']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) for an OpenAPI document term using JSON serialization.',
		argnames is ['Sink', 'Document']
	]).

	:- public(validate_document/1).
	:- mode(validate_document(+term), zero_or_one).
	:- info(validate_document/1, [
		comment is 'Succeeds if the given OpenAPI document term is valid according to the phase-1 OpenAPI validation schema.',
		argnames is ['Document']
	]).

	:- public(validate_document/2).
	:- mode(validate_document(+term, --list), one).
	:- info(validate_document/2, [
		comment is 'Validates an OpenAPI document term and returns a list of validation errors (empty list if valid).',
		argnames is ['Document', 'Errors']
	]).

	:- public(operation/3).
	:- mode(operation(+object_identifier, +atom, -compound), one_or_error).
	:- info(operation/3, [
		comment is 'Resolves an operation descriptor exposed by a provider object using its ``operationId`` atom.',
		argnames is ['Provider', 'OperationId', 'Operation']
	]).

	:- public(validate_request/3).
	:- mode(validate_request(+object_identifier, +atom, ++term), zero_or_one).
	:- info(validate_request/3, [
		comment is 'Succeeds when the given normalized HTTP request term or object implementing the ``http_request_protocol`` protocol conforms to the provider operation descriptor identified by the given ``operationId``.',
		argnames is ['Provider', 'OperationId', 'Request']
	]).

	:- public(validate_request/4).
	:- mode(validate_request(+object_identifier, +atom, ++term, --list), one).
	:- info(validate_request/4, [
		comment is 'Validates a normalized HTTP request term or an object implementing the ``http_request_protocol`` protocol against the provider operation descriptor identified by the given ``operationId`` and returns any contract errors.',
		argnames is ['Provider', 'OperationId', 'Request', 'Errors']
	]).

	:- public(validate_http_request/3).
	:- mode(validate_http_request(+object_identifier, +atom, ++compound), zero_or_one).
	:- info(validate_http_request/3, [
		comment is 'Succeeds when an HTTP request wire source parsed by the ``http_core`` library conforms to the provider operation descriptor identified by the given ``operationId``.',
		argnames is ['Provider', 'OperationId', 'Source']
	]).

	:- public(validate_http_request/4).
	:- mode(validate_http_request(+object_identifier, +atom, ++compound, --list), one).
	:- info(validate_http_request/4, [
		comment is 'Parses an HTTP request wire source using the ``http_core`` library and validates the resulting normalized request against the provider operation descriptor identified by the given ``operationId``.',
		argnames is ['Provider', 'OperationId', 'Source', 'Errors']
	]).

	:- public(validate_response/3).
	:- mode(validate_response(+object_identifier, +atom, ++term), zero_or_one).
	:- info(validate_response/3, [
		comment is 'Succeeds when the given normalized HTTP response term or object implementing the ``http_response_protocol`` protocol conforms to the provider operation descriptor identified by the given ``operationId``.',
		argnames is ['Provider', 'OperationId', 'Response']
	]).

	:- public(validate_response/4).
	:- mode(validate_response(+object_identifier, +atom, ++term, --list), one).
	:- info(validate_response/4, [
		comment is 'Validates a normalized HTTP response term or an object implementing the ``http_response_protocol`` protocol against the provider operation descriptor identified by the given ``operationId`` and returns any contract errors.',
		argnames is ['Provider', 'OperationId', 'Response', 'Errors']
	]).

	:- public(validate_http_response/3).
	:- mode(validate_http_response(+object_identifier, +atom, ++compound), zero_or_one).
	:- info(validate_http_response/3, [
		comment is 'Succeeds when an HTTP response wire source parsed by the ``http_core`` library conforms to the provider operation descriptor identified by the given ``operationId``.',
		argnames is ['Provider', 'OperationId', 'Source']
	]).

	:- public(validate_http_response/4).
	:- mode(validate_http_response(+object_identifier, +atom, ++compound, --list), one).
	:- info(validate_http_response/4, [
		comment is 'Parses an HTTP response wire source using the ``http_core`` library and validates the resulting normalized response against the provider operation descriptor identified by the given ``operationId``.',
		argnames is ['Provider', 'OperationId', 'Source', 'Errors']
	]).

	% top-level orchestration

	document(Provider, Document) :-
		once(derive_document(Provider, Document)).

	derive_document(Provider, Document) :-
		provider_info_object(Provider, Info),
		provider_servers_array(Provider, Servers),
		provider_security_pairs(Provider, SecurityPairs),
		provider_paths_object(Provider, Paths),
		provider_components_object(Provider, Components),
		provider_external_docs_pairs(Provider, ExternalDocsPairs),
		append(SecurityPairs, ExternalDocsPairs, OptionalPairs),
		Pairs = [openapi-'3.1.0', info-Info, servers-Servers, paths-Paths, components-Components| OptionalPairs],
		^^pairs_to_object(Pairs, Document).

	parse(Source, Document) :-
		json_parse(Source, Document).

	generate(Sink, Document) :-
		json_generate(Sink, Document).

	validate_document(Document) :-
		validate_document(Document, Errors),
		Errors == [].

	validate_document(Document, Errors) :-
		once(validate_document_errors(Document, Errors)).

	validate_document_errors(Document, Errors) :-
		open_api_document_schema(Schema),
		json_schema_validate(Schema, Document, StructuralErrors),
		validate_document_known_fields(Document, KnownFieldErrors),
		validate_document_security_schemes(Document, SecuritySchemeErrors),
		validate_document_security_references(Document, SecurityReferenceErrors),
		validate_document_paths(Document, PathErrors),
		append(StructuralErrors, KnownFieldErrors, Errors0),
		append(Errors0, SecuritySchemeErrors, Errors1),
		append(Errors1, SecurityReferenceErrors, Errors2),
		append(Errors2, PathErrors, Errors).

	validate_document_known_fields(Document, Errors) :-
		(	^^json_object_pairs(Document, DocumentPairs) ->
			validate_allowed_object_pairs(DocumentPairs, [], [openapi, info, jsonSchemaDialect, servers, paths, webhooks, components, security, tags, externalDocs], RootErrors),
			validate_document_info_known_fields(DocumentPairs, InfoErrors),
			validate_document_server_known_fields(DocumentPairs, ServerErrors),
			validate_document_components_known_fields(DocumentPairs, ComponentErrors),
			validate_document_external_docs_known_fields(DocumentPairs, ExternalDocsErrors),
			append(RootErrors, InfoErrors, Errors0),
			append(Errors0, ServerErrors, Errors1),
			append(Errors1, ComponentErrors, Errors2),
			append(Errors2, ExternalDocsErrors, Errors)
		;	Errors = []
		).
	validate_document_info_known_fields(DocumentPairs, Errors) :-
		(	lookup_pair_value(info, DocumentPairs, Info),
			^^json_object_pairs(Info, InfoPairs) ->
			validate_allowed_object_pairs(InfoPairs, [info], [title, summary, description, termsOfService, contact, license, version], InfoErrors),
			validate_document_contact_known_fields(InfoPairs, ContactErrors),
			validate_document_license_known_fields(InfoPairs, LicenseErrors),
			append(InfoErrors, ContactErrors, Errors0),
			append(Errors0, LicenseErrors, Errors)
		;	Errors = []
		).

	validate_document_contact_known_fields(InfoPairs, Errors) :-
		(	lookup_pair_value(contact, InfoPairs, Contact),
			^^json_object_pairs(Contact, ContactPairs) ->
			validate_allowed_object_pairs(ContactPairs, [info, contact], [name, url, email], Errors)
		;	Errors = []
		).

	validate_document_license_known_fields(InfoPairs, Errors) :-
		(	lookup_pair_value(license, InfoPairs, License),
			^^json_object_pairs(License, LicensePairs) ->
			validate_allowed_object_pairs(LicensePairs, [info, license], [name, identifier, url], Errors)
		;	Errors = []
		).

	validate_document_server_known_fields(DocumentPairs, Errors) :-
		(	lookup_pair_value(servers, DocumentPairs, Servers) ->
			validate_document_server_array_known_fields(Servers, 0, Errors)
		;	Errors = []
		).

	validate_document_server_array_known_fields([], _, []) :-
		!.
	validate_document_server_array_known_fields([Server| Servers], Index, Errors) :-
		(	^^json_object_pairs(Server, ServerPairs) ->
			validate_allowed_object_pairs(ServerPairs, [servers, Index], [url, description, variables], ServerErrors)
		;	ServerErrors = []
		),
		NextIndex is Index + 1,
		validate_document_server_array_known_fields(Servers, NextIndex, RestErrors),
		append(ServerErrors, RestErrors, Errors).
	validate_document_server_array_known_fields(_, _, []).

	validate_document_components_known_fields(DocumentPairs, Errors) :-
		(	lookup_pair_value(components, DocumentPairs, Components),
			^^json_object_pairs(Components, ComponentPairs) ->
			validate_allowed_object_pairs(ComponentPairs, [components], [schemas, responses, parameters, examples, requestBodies, headers, securitySchemes, links, callbacks, pathItems], KnownFieldErrors),
			validate_document_component_member_maps(ComponentPairs, MemberNameErrors),
			append(KnownFieldErrors, MemberNameErrors, Errors)
		;	Errors = []
		).

	validate_document_component_member_maps([], []).
	validate_document_component_member_maps([Pair| Pairs], Errors) :-
		^^pair_key_value(Pair, Key, Value),
		(	extension_key(Key) ->
			CurrentErrors = []
		;	validate_document_component_member_map(Key, Value, CurrentErrors)
		),
		validate_document_component_member_maps(Pairs, RestErrors),
		append(CurrentErrors, RestErrors, Errors).

	validate_document_component_member_map(Key, Value, Errors) :-
		(	^^json_object_pairs(Value, Pairs) ->
			validate_document_component_member_pairs(Pairs, [components, Key], Errors)
		;	Errors = []
		).

	validate_document_component_member_pairs([], _, []).
	validate_document_component_member_pairs([Pair| Pairs], Context, Errors) :-
		^^pair_key_value(Pair, Name, _),
		append(Context, [Name], Path),
		(	valid_component_member_name(Name) ->
			CurrentErrors = []
		;	CurrentErrors = [error(Path, invalid_component_name(Name))]
		),
		validate_document_component_member_pairs(Pairs, Context, RestErrors),
		append(CurrentErrors, RestErrors, Errors).

	validate_document_external_docs_known_fields(DocumentPairs, Errors) :-
		(	lookup_pair_value(externalDocs, DocumentPairs, ExternalDocs),
			^^json_object_pairs(ExternalDocs, ExternalDocsPairs) ->
			validate_allowed_object_pairs(ExternalDocsPairs, [externalDocs], [description, url], Errors)
		;	Errors = []
		).

	validate_allowed_object_pairs(Pairs, Context, Allowed, Errors) :-
		validate_allowed_object_pairs(Pairs, Context, Allowed, [], ReversedErrors),
		reverse(ReversedErrors, Errors).

	validate_allowed_object_pairs([], _, _, Errors, Errors).
	validate_allowed_object_pairs([Pair| Pairs], Context, Allowed, Errors0, Errors) :-
		^^pair_key_value(Pair, Name, _),
		(	allowed_object_key(Name, Allowed) ->
			Errors1 = Errors0
		;	append(Context, [Name], Path),
			Errors1 = [error(Path, unexpected_property)| Errors0]
		),
		validate_allowed_object_pairs(Pairs, Context, Allowed, Errors1, Errors).

	allowed_object_key(Name, Allowed) :-
		memberchk(Name, Allowed),
		!.
	allowed_object_key(Name, _) :-
		extension_key(Name).

	validate_document_security_schemes(Document, Errors) :-
		(	document_security_scheme_pairs(Document, SecuritySchemePairs) ->
			validate_document_security_scheme_pairs(SecuritySchemePairs, [components, securitySchemes], Errors)
		;	Errors = []
		).

	validate_document_security_references(Document, Errors) :-
		document_security_scheme_pairs_or_empty(Document, SecuritySchemePairs),
		validate_document_root_security_references(Document, SecuritySchemePairs, RootErrors),
		validate_document_operation_security_references(Document, SecuritySchemePairs, OperationErrors),
		append(RootErrors, OperationErrors, Errors).

	document_security_scheme_pairs_or_empty(Document, SecuritySchemePairs) :-
		(	document_security_scheme_pairs(Document, SecuritySchemePairs) ->
			true
		;	SecuritySchemePairs = []
		).

	validate_document_root_security_references(Document, SecuritySchemePairs, Errors) :-
		(	^^json_object_pairs(Document, DocumentPairs),
			lookup_pair_value(security, DocumentPairs, Requirements) ->
			validate_document_security_requirement_array(Requirements, [security], SecuritySchemePairs, Errors)
		;	Errors = []
		).

	validate_document_operation_security_references(Document, SecuritySchemePairs, Errors) :-
		(	document_path_pairs(Document, PathPairs) ->
			validate_document_operation_security_path_pairs(PathPairs, SecuritySchemePairs, Errors)
		;	Errors = []
		).

	validate_document_operation_security_path_pairs([], _, []).
	validate_document_operation_security_path_pairs([Pair| Pairs], SecuritySchemePairs, Errors) :-
		^^pair_key_value(Pair, Path, PathItem),
		(	extension_key(Path) ->
			PathErrors = []
		;	validate_document_operation_security_path_item(PathItem, Path, SecuritySchemePairs, PathErrors)
		),
		validate_document_operation_security_path_pairs(Pairs, SecuritySchemePairs, RestErrors),
		append(PathErrors, RestErrors, Errors).

	validate_document_operation_security_path_item(PathItem, Path, SecuritySchemePairs, Errors) :-
		(	^^json_object_pairs(PathItem, PathItemPairs) ->
			validate_document_operation_security_pairs(PathItemPairs, Path, SecuritySchemePairs, Errors)
		;	Errors = []
		).

	validate_document_operation_security_pairs([], _, _, []).
	validate_document_operation_security_pairs([Pair| Pairs], Path, SecuritySchemePairs, Errors) :-
		^^pair_key_value(Pair, Key, Operation),
		(	operation_method(Key),
			^^json_object_pairs(Operation, OperationPairs),
			lookup_pair_value(security, OperationPairs, Requirements) ->
			Context = [paths, Path, Key, security],
			validate_document_security_requirement_array(Requirements, Context, SecuritySchemePairs, OperationErrors)
		;	OperationErrors = []
		),
		validate_document_operation_security_pairs(Pairs, Path, SecuritySchemePairs, RestErrors),
		append(OperationErrors, RestErrors, Errors).

	validate_document_security_requirement_array([], _, _, []) :-
		!.
	validate_document_security_requirement_array([Requirement| Requirements], Context, SecuritySchemePairs, Errors) :-
		(	^^json_object_pairs(Requirement, RequirementPairs) ->
			validate_document_security_requirement_pairs(RequirementPairs, Context, SecuritySchemePairs, RequirementErrors)
		;	RequirementErrors = [error(Context, expected_type(object))]
		),
		validate_document_security_requirement_array(Requirements, Context, SecuritySchemePairs, RestErrors),
		append(RequirementErrors, RestErrors, Errors).
	validate_document_security_requirement_array(_, Context, _, [error(Context, expected_type(array))]).

	validate_document_security_requirement_pairs([], _, _, []) :-
		!.
	validate_document_security_requirement_pairs([Scheme-Scopes| Requirements], Context, SecuritySchemePairs, Errors) :-
		append(Context, [Scheme], SchemePath),
		(	lookup_pair_value(Scheme, SecuritySchemePairs, SecurityScheme) ->
			validate_document_security_requirement_scopes(Scheme, SecurityScheme, Scopes, SchemePath, ScopeErrors)
		;	ScopeErrors = [error(SchemePath, existence_error(open_api_security_scheme, Scheme))]
		),
		validate_document_security_requirement_pairs(Requirements, Context, SecuritySchemePairs, RestErrors),
		append(ScopeErrors, RestErrors, Errors).

	validate_document_security_requirement_scopes(Scheme, SecurityScheme, Scopes, Path, Errors) :-
		(	catch(validate_security_requirement_scopes(Scheme, SecurityScheme, Scopes), Error, true) ->
			(	var(Error) ->
				Errors = []
			;	Errors = [error(Path, Error)]
			)
		;	Errors = []
		).

	validate_document_paths(Document, Errors) :-
		(	document_path_pairs(Document, PathPairs) ->
			validate_document_path_pairs(PathPairs, [], Errors)
		;	Errors = []
		).

	document_path_pairs(Document, PathPairs) :-
		^^json_object_pairs(Document, DocumentPairs),
		lookup_pair_value(paths, DocumentPairs, Paths),
		^^json_object_pairs(Paths, PathPairs).

	validate_document_path_pairs([], _, []).
	validate_document_path_pairs([Pair| Pairs], SeenSignatures, Errors) :-
		^^pair_key_value(Pair, Path, PathItem),
		(	extension_key(Path) ->
			PathErrors = [],
			SeenSignatures1 = SeenSignatures
		;	validate_document_path_name(Path, SeenSignatures, SeenSignatures1, NameErrors),
			(	NameErrors == [] ->
				validate_document_path_item(Path, PathItem, ItemErrors)
			;	ItemErrors = []
			),
			append(NameErrors, ItemErrors, PathErrors)
		),
		append(PathErrors, RestErrors, Errors),
		validate_document_path_pairs(Pairs, SeenSignatures1, RestErrors).

	validate_document_path_name(Path, SeenSignatures, SeenSignatures, [error([paths, Path], invalid_path_name)]) :-
		(	\+ atom(Path)
		;	\+ sub_atom(Path, 0, 1, _, '/')
		),
		!.
	validate_document_path_name(Path, SeenSignatures, SeenSignatures, [error([paths, Path], duplicate_templated_path(SeenPath))]) :-
		path_template_signature(Path, Signature),
		memberchk(Signature-SeenPath, SeenSignatures),
		SeenPath \== Path,
		!.
	validate_document_path_name(Path, SeenSignatures, [Signature-Path| SeenSignatures], []) :-
		path_template_signature(Path, Signature).

	validate_document_path_item(Path, PathItem, Errors) :-
		(	^^json_object_pairs(PathItem, PathItemPairs) ->
			path_template_parameter_names(Path, TemplateNames),
			validate_document_parameter_definitions(PathItemPairs, [paths, Path, parameters], ParameterDefinitionErrors),
			document_object_path_parameter_names(PathItemPairs, TemplateNames, [paths, Path, parameters], PathItemNames, PathItemReferences, PathItemErrors),
			validate_document_path_item_operations(PathItemPairs, Path, TemplateNames, PathItemNames, PathItemReferences, OperationErrors),
			append(ParameterDefinitionErrors, PathItemErrors, Errors0),
			append(Errors0, OperationErrors, Errors)
		;	Errors = [error([paths, Path], expected_type(object))]
		).

	validate_document_path_item_operations([], _, _, _, _, []).
	validate_document_path_item_operations([Pair| Pairs], Path, TemplateNames, PathItemNames, PathItemReferences, Errors) :-
		^^pair_key_value(Pair, Key, Operation),
		(	operation_method(Key) ->
			validate_document_path_operation(Path, Key, Operation, TemplateNames, PathItemNames, PathItemReferences, OperationErrors)
		;	OperationErrors = []
		),
		append(OperationErrors, RestErrors, Errors),
		validate_document_path_item_operations(Pairs, Path, TemplateNames, PathItemNames, PathItemReferences, RestErrors).

	validate_document_path_operation(Path, Method, Operation, TemplateNames, PathItemNames, PathItemReferences, Errors) :-
		(	^^json_object_pairs(Operation, OperationPairs) ->
			validate_document_parameter_definitions(OperationPairs, [paths, Path, Method, parameters], ParameterDefinitionErrors),
			document_object_path_parameter_names(OperationPairs, TemplateNames, [paths, Path, Method, parameters], OperationNames, OperationReferences, ParameterErrors),
			merge_unique_names(PathItemNames, OperationNames, EffectiveNames),
			merge_document_reference_flags(PathItemReferences, OperationReferences, ReferencesPresent),
			(	ReferencesPresent == yes ->
				CoverageErrors = []
			;	validate_document_path_parameter_coverage(TemplateNames, EffectiveNames, [paths, Path, Method, parameters], CoverageErrors)
			),
			append(ParameterDefinitionErrors, ParameterErrors, Errors0),
			append(Errors0, CoverageErrors, Errors)
		;	Errors = [error([paths, Path, Method], expected_type(object))]
		).

	validate_document_parameter_definitions(Pairs, Context, Errors) :-
		(	lookup_pair_value(parameters, Pairs, Parameters) ->
			validate_document_parameter_array_definitions(Parameters, Context, [], Errors)
		;	Errors = []
		).

	validate_document_parameter_array_definitions([], _, _, []) :-
		!.
	validate_document_parameter_array_definitions([Parameter| Parameters], Context, Seen0, Errors) :-
		(	document_parameter_key(Parameter, Name-In) ->
			(	memberchk(Name-In, Seen0) ->
				CurrentErrors = [error(Context, duplicate_parameter(Name, In))],
				Seen1 = Seen0
			;	CurrentErrors = [],
				Seen1 = [Name-In| Seen0]
			)
		;	CurrentErrors = [],
			Seen1 = Seen0
		),
		validate_document_parameter_array_definitions(Parameters, Context, Seen1, RestErrors),
		append(CurrentErrors, RestErrors, Errors).
	validate_document_parameter_array_definitions(_, _, _, []).

	document_parameter_key(Parameter, Name-In) :-
		^^json_object_pairs(Parameter, ParameterPairs),
		\+ lookup_pair_value('$ref', ParameterPairs, _),
		lookup_pair_value(name, ParameterPairs, Name),
		atom(Name),
		lookup_pair_value(in, ParameterPairs, In),
		atom(In),
		\+ ignored_parameter(Name, In).

	document_object_path_parameter_names(Pairs, TemplateNames, Context, Names, ReferencesPresent, Errors) :-
		(	lookup_pair_value(parameters, Pairs, Parameters) ->
			document_path_parameter_array(Parameters, TemplateNames, Context, [], Names, no, ReferencesPresent, Errors)
		;	Names = [],
			ReferencesPresent = no,
			Errors = []
		).

	document_path_parameter_array([], _, _, Names, Names, ReferencesPresent, ReferencesPresent, []).
	document_path_parameter_array([Parameter| Parameters], TemplateNames, Context, Names0, Names, ReferencesPresent0, ReferencesPresent, Errors) :-
		document_path_parameter_entry(Parameter, TemplateNames, Context, Names0, Names1, EntryReferencesPresent, EntryErrors),
		merge_document_reference_flags(ReferencesPresent0, EntryReferencesPresent, ReferencesPresent1),
		document_path_parameter_array(Parameters, TemplateNames, Context, Names1, Names, ReferencesPresent1, ReferencesPresent, RestErrors),
		append(EntryErrors, RestErrors, Errors).
	document_path_parameter_array(Parameters, _, Context, Names, Names, ReferencesPresent, ReferencesPresent, [error(Context, expected_type(array))]) :-
		Parameters \== [],
		Parameters \== [_| _].

	document_path_parameter_entry(Parameter, _, _, Names, Names, yes, []) :-
		^^json_object_pairs(Parameter, ParameterPairs),
		lookup_pair_value('$ref', ParameterPairs, _),
		!.
	document_path_parameter_entry(Parameter, TemplateNames, Context, Names0, Names, no, Errors) :-
		(	^^json_object_pairs(Parameter, ParameterPairs) ->
			(	lookup_pair_value(in, ParameterPairs, path) ->
				document_inline_path_parameter(ParameterPairs, TemplateNames, Context, Names0, Names, Errors)
			;	Names = Names0,
				Errors = []
			)
		;	Names = Names0,
			Errors = [error(Context, expected_type(object))]
		).

	document_inline_path_parameter(ParameterPairs, TemplateNames, Context, Names0, Names, Errors) :-
		document_path_parameter_name(ParameterPairs, Context, Name, NameErrors),
		document_path_parameter_required(ParameterPairs, Context, Name, RequiredErrors),
		(	NameErrors == [] ->
			validate_document_path_parameter_name(Name, TemplateNames, Context, NameValidationErrors),
			insert_unique_name(Name, Names0, Names)
		;	NameValidationErrors = [],
			Names = Names0
		),
		append(NameErrors, RequiredErrors, Errors0),
		append(Errors0, NameValidationErrors, Errors).

	document_path_parameter_name(ParameterPairs, _, Name, []) :-
		lookup_pair_value(name, ParameterPairs, Name),
		atom(Name),
		!.
	document_path_parameter_name(_, Context, _, [error(Context, missing_required(name))]).

	document_path_parameter_required(ParameterPairs, _, _, []) :-
		lookup_pair_value(required, ParameterPairs, Required),
		json_true(Required),
		!.
	document_path_parameter_required(_, Context, Name, [error(Path, missing_required(true))]) :-
		append(Context, [Name], Path).

	validate_document_path_parameter_name(Name, TemplateNames, _, []) :-
		memberchk(Name, TemplateNames),
		!.
	validate_document_path_parameter_name(Name, _, Context, [error(Path, not_in_path_template)]) :-
		append(Context, [Name], Path).

	validate_document_path_parameter_coverage([], _, _, []).
	validate_document_path_parameter_coverage([Name| Names], DeclaredNames, Context, Errors) :-
		(	memberchk(Name, DeclaredNames) ->
			CurrentErrors = []
		;	CurrentErrors = [error(Context, missing_path_parameter(Name))]
		),
		validate_document_path_parameter_coverage(Names, DeclaredNames, Context, RestErrors),
		append(CurrentErrors, RestErrors, Errors).

	merge_document_reference_flags(yes, _, yes) :-
		!.
	merge_document_reference_flags(_, yes, yes) :-
		!.
	merge_document_reference_flags(_, _, no).

	merge_unique_names([], Names, Names).
	merge_unique_names([Name| Names], Names0, MergedNames) :-
		insert_unique_name(Name, Names0, Names1),
		merge_unique_names(Names, Names1, MergedNames).

	insert_unique_name(Name, Names, Names) :-
		memberchk(Name, Names),
		!.
	insert_unique_name(Name, Names, [Name| Names]).

	document_security_scheme_pairs(Document, SecuritySchemePairs) :-
		^^json_object_pairs(Document, DocumentPairs),
		lookup_pair_value(components, DocumentPairs, Components),
		^^json_object_pairs(Components, ComponentPairs),
		lookup_pair_value(securitySchemes, ComponentPairs, SecuritySchemes),
		^^json_object_pairs(SecuritySchemes, SecuritySchemePairs).

	validate_document_security_scheme_pairs([], _, []).
	validate_document_security_scheme_pairs([Pair| Pairs], Path, Errors) :-
		^^pair_key_value(Pair, Name, SecurityScheme),
		append(Path, [Name], SecuritySchemePath),
		validate_document_security_scheme(SecurityScheme, SecuritySchemePath, SecuritySchemeErrors),
		append(SecuritySchemeErrors, RestErrors, Errors),
		validate_document_security_scheme_pairs(Pairs, Path, RestErrors).

	validate_document_security_scheme(SecurityScheme, Path, Errors) :-
		(	^^json_object_pairs(SecurityScheme, Pairs) ->
			validate_allowed_object_pairs(Pairs, Path, [type, description, name, in, scheme, bearerFormat, flows, openIdConnectUrl], PropertyErrors),
			validate_document_security_scheme_type(Pairs, Path, Type, TypeErrors),
			validate_document_security_scheme_type_errors(Type, Pairs, Path, FieldErrors),
			append(PropertyErrors, TypeErrors, Errors0),
			append(Errors0, FieldErrors, Errors)
		;	Errors = [error(Path, expected_type(object))]
		).

	validate_document_security_scheme_type(Pairs, Path, Type, Errors) :-
		(	lookup_pair_value(type, Pairs, Type) ->
			(	valid_security_scheme_type(Type) ->
				Errors = []
			;	Errors = [error(Path, invalid_security_scheme_type(Type))]
			)
		;	Type = invalid,
			Errors = [error(Path, missing_required(type))]
		).

	valid_security_scheme_type(apiKey).
	valid_security_scheme_type(http).
	valid_security_scheme_type(mutualTLS).
	valid_security_scheme_type(oauth2).
	valid_security_scheme_type(openIdConnect).

	validate_document_security_scheme_type_errors(invalid, _, _, []).
	validate_document_security_scheme_type_errors(apiKey, Pairs, Path, Errors) :-
		check_required_pairs([name, in], Pairs, Path, Errors).
	validate_document_security_scheme_type_errors(http, Pairs, Path, Errors) :-
		check_required_pairs([scheme], Pairs, Path, Errors).
	validate_document_security_scheme_type_errors(mutualTLS, _, _, []).
	validate_document_security_scheme_type_errors(oauth2, Pairs, Path, Errors) :-
		required_pair_value(Pairs, Path, flows, Flows, FlowPresenceErrors),
		append(Path, [flows], FlowPath),
		(	FlowPresenceErrors == [] ->
			validate_document_oauth_flows(Flows, FlowPath, FlowErrors)
		;	FlowErrors = []
		),
		append(FlowPresenceErrors, FlowErrors, Errors).
	validate_document_security_scheme_type_errors(openIdConnect, Pairs, Path, Errors) :-
		required_pair_value(Pairs, Path, openIdConnectUrl, _, OpenIdErrors),
		validate_allowed_object_pairs(Pairs, Path, [type, description, openIdConnectUrl], PropertyErrors),
		append(OpenIdErrors, PropertyErrors, Errors).

	check_required_pairs([], _, _, []).
	check_required_pairs([Field| Fields], Pairs, Path, Errors) :-
		required_pair_value(Pairs, Path, Field, _, FieldErrors),
		append(FieldErrors, RestErrors, Errors),
		check_required_pairs(Fields, Pairs, Path, RestErrors).

	required_pair_value(Pairs, _Path, Field, Value, []) :-
		lookup_pair_value(Field, Pairs, Value),
		!.
	required_pair_value(_, Path, Field, _, [error(Path, missing_required(Field))]).

	validate_document_oauth_flows(Flows, Path, Errors) :-
		(	^^json_object_pairs(Flows, FlowPairs) ->
			validate_allowed_object_pairs(FlowPairs, Path, [implicit, password, clientCredentials, authorizationCode], FlowNameErrors),
			validate_document_oauth_flow_pairs(FlowPairs, Path, 0, Count, FlowErrors),
			(	Count =:= 0 ->
				Errors0 = [error(Path, missing_required(oauth_flow))| FlowErrors]
			;	Errors0 = FlowErrors
			),
			append(FlowNameErrors, Errors0, Errors)
		;	Errors = [error(Path, expected_type(object))]
		).

	validate_document_oauth_flow_pairs([], _, Count, Count, []).
	validate_document_oauth_flow_pairs([Pair| Pairs], Path, Count0, Count, Errors) :-
		^^pair_key_value(Pair, FlowName, FlowObject),
		(	oauth_flow_name(FlowName) ->
			append(Path, [FlowName], FlowPath),
			validate_document_oauth_flow(FlowName, FlowObject, FlowPath, FlowErrors),
			Count1 is Count0 + 1
		;	FlowErrors = [],
			Count1 is Count0
		),
		append(FlowErrors, RestErrors, Errors),
		validate_document_oauth_flow_pairs(Pairs, Path, Count1, Count, RestErrors).

	validate_document_oauth_flow(FlowName, FlowObject, Path, Errors) :-
		(	^^json_object_pairs(FlowObject, FlowPairs) ->
			validate_allowed_object_pairs(FlowPairs, Path, [authorizationUrl, tokenUrl, refreshUrl, scopes], PropertyErrors),
			required_oauth_flow_fields(FlowName, RequiredFields),
			check_required_pairs(RequiredFields, FlowPairs, Path, FieldErrors),
			append(PropertyErrors, FieldErrors, Errors)
		;	Errors = [error(Path, expected_type(object))]
		).

	required_oauth_flow_fields(implicit, [authorizationUrl, scopes]).
	required_oauth_flow_fields(password, [tokenUrl, scopes]).
	required_oauth_flow_fields(clientCredentials, [tokenUrl, scopes]).
	required_oauth_flow_fields(authorizationCode, [authorizationUrl, tokenUrl, scopes]).

	operation(Provider, OperationId, Operation) :-
		Provider::operations(Operations),
		check_unique_operation_ids(Operations),
		operation_descriptor_by_id(Operations, OperationId, Operation).

	validate_request(Provider, OperationId, Request) :-
		validate_request(Provider, OperationId, Request, Errors),
		Errors == [].

	validate_request(Provider, OperationId, Request, Errors) :-
		operation(Provider, OperationId, Operation),
		validate_request_against_operation(Provider, Operation, Request, Errors).

	validate_http_request(Provider, OperationId, Source) :-
		validate_http_request(Provider, OperationId, Source, Errors),
		Errors == [].

	validate_http_request(Provider, OperationId, Source, Errors) :-
		http_parse_request(Source, Request),
		validate_request(Provider, OperationId, Request, Errors).

	validate_response(Provider, OperationId, Response) :-
		validate_response(Provider, OperationId, Response, Errors),
		Errors == [].

	validate_response(Provider, OperationId, Response, Errors) :-
		operation(Provider, OperationId, Operation),
		validate_response_against_operation(Provider, Operation, Response, Errors).

	validate_http_response(Provider, OperationId, Source) :-
		validate_http_response(Provider, OperationId, Source, Errors),
		Errors == [].

	validate_http_response(Provider, OperationId, Source, Errors) :-
		http_parse_response(Source, Response),
		validate_response(Provider, OperationId, Response, Errors).

	% operation lookup and contract validation

	operation_descriptor_by_id(Operations, OperationId, Operation) :-
		operation_descriptor_by_id_matches(Operations, OperationId, Matches),
		(	Matches = [Operation] ->
			true
		;	Matches == [] ->
			existence_error(open_api_operation, OperationId)
		;	domain_error(open_api_operation_id, duplicate(OperationId))
		).

	operation_descriptor_by_id_matches([], _, []).
	operation_descriptor_by_id_matches([Descriptor| Descriptors], OperationId, Matches) :-
		(	Descriptor = operation(OperationId, _, _, _, _, _, _, _) ->
			Matches = [Descriptor| Matches0]
		;	Matches = Matches0
		),
		operation_descriptor_by_id_matches(Descriptors, OperationId, Matches0).

	validate_request_against_operation(Provider, Operation, Request, Errors) :-
		(	request_message_data(Request, Method, Target, Headers, Body, Properties) ->
			Operation = operation(_, ExpectedMethod, ExpectedPath, _, Parameters, RequestBody, _, _),
			validate_request_method(ExpectedMethod, Method, MethodErrors),
			validate_request_target(ExpectedPath, Target, Properties, PathParameters, TargetErrors),
			validate_request_parameters(Provider, Parameters, Headers, Properties, PathParameters, ParameterErrors),
			validate_request_body(Provider, RequestBody, Body, BodyErrors),
			append(MethodErrors, TargetErrors, Errors0),
			append(Errors0, ParameterErrors, Errors1),
			append(Errors1, BodyErrors, Errors)
		;	Errors = [invalid_request(Request)]
		).

	validate_response_against_operation(Provider, Operation, Response, Errors) :-
		Operation = operation(_, _, _, _, _, _, Responses, _),
		(	response_message_data(Response, Status, Body) ->
			(	status_code(Status, StatusCode) ->
				(	response_descriptor_for_status(Responses, StatusCode, Descriptor) ->
					validate_response_body(Provider, StatusCode, Descriptor, Body, Errors)
				;	Errors = [unexpected_response_status(StatusCode)]
				)
			;	Errors = [invalid_response_status(Status)]
			)
		;	Errors = [invalid_response(Response)]
		).

	request_message_data(Request, Method, Target, Headers, Body, Properties) :-
		(	http_is_request(Request) ->
			http_method(Request, Method),
			http_target(Request, Target),
			http_headers(Request, Headers),
			http_body(Request, Body),
			findall(Property, http_property(Request, Property), Properties)
		;	(	catch(conforms_to_protocol(Request, http_request_protocol), _, fail) ->
				Request::method(Method),
				Request::target(Target),
				Request::headers(Headers),
				Request::body(Body),
				findall(Property, Request::property(Property), Properties)
			;	fail
			)
		).

	response_message_data(Response, Status, Body) :-
		(	http_is_response(Response) ->
			http_status(Response, Status),
			http_body(Response, Body)
		;	(	catch(conforms_to_protocol(Response, http_response_protocol), _, fail) ->
				Response::status(Status),
				Response::body(Body)
			;	fail
			)
		).

	validate_request_method(ExpectedMethod, ExpectedMethod, []) :-
		!.
	validate_request_method(ExpectedMethod, Method, [request_method_mismatch(ExpectedMethod, Method)]).

	validate_request_target(ExpectedPath, Target, Properties, PathParameters, Errors) :-
		path_parameters_property(Properties, ProvidedPathParameters),
		(	request_target_path(Target, ActualPath) ->
			(	path_template_parameter_pairs(ExpectedPath, ActualPath, DerivedPathParameters) ->
				reconcile_path_parameters(DerivedPathParameters, ProvidedPathParameters, PathParameters, Errors)
			;	PathParameters = ProvidedPathParameters,
				Errors = [request_path_mismatch(ExpectedPath, ActualPath)]
			)
		;	PathParameters = ProvidedPathParameters,
			Errors = [invalid_request_target(Target)]
		).

	validate_request_parameters(Provider, Parameters, Headers, Properties, PathParameters, Errors) :-
		query_pairs_property(Properties, QueryPairs),
		cookie_pairs_property(Properties, CookiePairs),
		validate_request_parameters(Parameters, Provider, Headers, QueryPairs, CookiePairs, PathParameters, Errors).

	validate_request_parameters([], _, _, _, _, _, []).
	validate_request_parameters([parameter(Name, In, _, _, _)| Parameters], Provider, Headers, QueryPairs, CookiePairs, PathParameters, Errors) :-
		ignored_parameter(Name, In),
		!,
		validate_request_parameters(Parameters, Provider, Headers, QueryPairs, CookiePairs, PathParameters, Errors).
	validate_request_parameters([Parameter| Parameters], Provider, Headers, QueryPairs, CookiePairs, PathParameters, Errors) :-
		validate_request_parameter(Provider, Parameter, Headers, QueryPairs, CookiePairs, PathParameters, ParameterErrors),
		validate_request_parameters(Parameters, Provider, Headers, QueryPairs, CookiePairs, PathParameters, RestErrors),
		append(ParameterErrors, RestErrors, Errors).

	validate_request_parameter(Provider, parameter(Name, In, _Description, Required, Schema), Headers, QueryPairs, CookiePairs, PathParameters, Errors) :-
		(	request_parameter_value(In, Name, Headers, QueryPairs, CookiePairs, PathParameters, Value) ->
			resolve_schema_for_validation(Provider, Schema, ValidationSchema),
			^^normalize_json_value(Value, JsonValue),
			json_schema_validate(ValidationSchema, JsonValue, SchemaErrors),
			(	SchemaErrors == [] ->
				Errors = []
			;	Errors = [invalid_parameter(In, Name, SchemaErrors)]
			)
		;	Required == true ->
			Errors = [missing_parameter(In, Name)]
		;	Errors = []
		).

	request_parameter_value(In, Name, Headers, QueryPairs, CookiePairs, PathParameters, Value) :-
		valid_parameter_location(In),
		!,
		request_parameter_value_at(In, Name, Headers, QueryPairs, CookiePairs, PathParameters, Value).
	request_parameter_value(In, _, _, _, _, _, _) :-
		domain_error(open_api_parameter_location, In).

	request_parameter_value_at(path, Name, _Headers, _QueryPairs, _CookiePairs, PathParameters, Value) :-
		lookup_pair_value(Name, PathParameters, Value),
		!.
	request_parameter_value_at(query, Name, _Headers, QueryPairs, _CookiePairs, _PathParameters, Value) :-
		lookup_pair_value(Name, QueryPairs, Value),
		!.
	request_parameter_value_at(header, Name, Headers, _QueryPairs, _CookiePairs, _PathParameters, Value) :-
		lookup_header_value(Name, Headers, Value),
		!.
	request_parameter_value_at(cookie, Name, _Headers, _QueryPairs, CookiePairs, _PathParameters, Value) :-
		lookup_pair_value(Name, CookiePairs, Value),
		!.

	lookup_header_value(Name, Headers, Value) :-
		atom(Name),
		ascii_lower_atom(Name, LowerName),
		lookup_header_value_lower(LowerName, Headers, Value).

	lookup_header_value_lower(_, Headers, _) :-
		var(Headers),
		!,
		fail.
	lookup_header_value_lower(Name, [Pair| _], Value) :-
		^^pair_key_value(Pair, HeaderName, Value),
		atom(HeaderName),
		ascii_lower_atom(HeaderName, LowerHeaderName),
		LowerHeaderName == Name,
		!.
	lookup_header_value_lower(Name, [_| Headers], Value) :-
		lookup_header_value_lower(Name, Headers, Value).

	valid_parameter_location(path).
	valid_parameter_location(query).
	valid_parameter_location(header).
	valid_parameter_location(cookie).

	validate_request_body(_, none, empty, []) :-
		!.
	validate_request_body(_, none, Body, [unexpected_request_body]) :-
		Body \== empty,
		!.
	validate_request_body(_, request_body(_, false, _), empty, []) :-
		!.
	validate_request_body(_, request_body(_, true, _), empty, [missing_request_body]) :-
		!.
	validate_request_body(Provider, request_body(_, _, MediaTypes), content(MediaType, Payload), Errors) :-
		!,
		(	media_descriptor_schema(MediaTypes, MediaType, Schema) ->
			validate_request_payload(Provider, MediaType, Payload, Schema, Errors)
		;	Errors = [unsupported_request_media_type(MediaType)]
		).
	validate_request_body(_, _, Body, [invalid_request_body_term(Body)]).

	validate_request_payload(Provider, MediaType, Payload, Schema, Errors) :-
		(	payload_json_value(Payload, JsonValue) ->
			resolve_schema_for_validation(Provider, Schema, ValidationSchema),
			json_schema_validate(ValidationSchema, JsonValue, SchemaErrors),
			(	SchemaErrors == [] ->
				Errors = []
			;	Errors = [invalid_request_body(MediaType, SchemaErrors)]
			)
		;	payload_type(Payload, PayloadType),
			Errors = [unsupported_request_body_payload(PayloadType)]
		).

	validate_response_body(_, _StatusCode, response(_, _, []), empty, []) :-
		!.
	validate_response_body(_, StatusCode, response(_, _, []), Body, [unexpected_response_body(StatusCode)]) :-
		Body \== empty,
		!.
	validate_response_body(_, StatusCode, response(_, _, [_| _]), empty, [missing_response_body(StatusCode)]) :-
		!.
	validate_response_body(Provider, StatusCode, response(_, _, MediaTypes), content(MediaType, Payload), Errors) :-
		!,
		(	media_descriptor_schema(MediaTypes, MediaType, Schema) ->
			validate_response_payload(Provider, StatusCode, MediaType, Payload, Schema, Errors)
		;	Errors = [unsupported_response_media_type(StatusCode, MediaType)]
		).
	validate_response_body(_, StatusCode, _, Body, [invalid_response_body_term(StatusCode, Body)]).

	validate_response_payload(Provider, StatusCode, MediaType, Payload, Schema, Errors) :-
		(	payload_json_value(Payload, JsonValue) ->
			resolve_schema_for_validation(Provider, Schema, ValidationSchema),
			json_schema_validate(ValidationSchema, JsonValue, SchemaErrors),
			(	SchemaErrors == [] ->
				Errors = []
			;	Errors = [invalid_response_body(StatusCode, MediaType, SchemaErrors)]
			)
		;	payload_type(Payload, PayloadType),
			Errors = [unsupported_response_body_payload(StatusCode, PayloadType)]
		).

	response_descriptor_for_status(Responses, StatusCode, Descriptor) :-
		exact_response_descriptor(Responses, StatusCode, Descriptor),
		!.
	response_descriptor_for_status(Responses, StatusCode, Descriptor) :-
		response_range_descriptor(Responses, StatusCode, Descriptor),
		!.
	response_descriptor_for_status(Responses, _, Descriptor) :-
		default_response_descriptor(Responses, Descriptor).

	exact_response_descriptor([response(Status, Description, MediaTypes)| _], StatusCode, response(Status, Description, MediaTypes)) :-
		integer(Status),
		Status =:= StatusCode,
		!.
	exact_response_descriptor([_| Responses], StatusCode, Descriptor) :-
		exact_response_descriptor(Responses, StatusCode, Descriptor).

	response_range_descriptor([response(Status, Description, MediaTypes)| _], StatusCode, response(Status, Description, MediaTypes)) :-
		response_status_range(Status, Minimum, Maximum),
		StatusCode >= Minimum,
		StatusCode =< Maximum,
		!.
	response_range_descriptor([_| Responses], StatusCode, Descriptor) :-
		response_range_descriptor(Responses, StatusCode, Descriptor).

	default_response_descriptor([response(default, Description, MediaTypes)| _], response(default, Description, MediaTypes)) :-
		!.
	default_response_descriptor([_| Responses], Descriptor) :-
		default_response_descriptor(Responses, Descriptor).

	status_code(status(Code, _), Code) :-
		integer(Code),
		!.
	status_code(Code, Code) :-
		integer(Code).

	media_descriptor_schema(MediaTypes, MediaType, Schema) :-
		media_type_signature(MediaType, Type, Subtype),
		media_descriptor_schema(MediaTypes, Type, Subtype, none, -1, Schema).

	media_descriptor_schema([], _, _, some(Schema), _, Schema).
	media_descriptor_schema([media(DeclaredMediaType, CandidateSchema)| MediaTypes], Type, Subtype, Best0, Score0, Schema) :-
		(	media_type_match_score(DeclaredMediaType, Type, Subtype, Score),
			Score > Score0 ->
			Best1 = some(CandidateSchema),
			Score1 = Score
		;	Best1 = Best0,
			Score1 = Score0
		),
		media_descriptor_schema(MediaTypes, Type, Subtype, Best1, Score1, Schema).

	media_type_match_score(DeclaredMediaType, Type, Subtype, 2) :-
		media_type_signature(DeclaredMediaType, Type, Subtype),
		!.
	media_type_match_score(DeclaredMediaType, Type, _, 1) :-
		media_type_signature(DeclaredMediaType, Type, '*'),
		!.
	media_type_match_score(DeclaredMediaType, _, _, 0) :-
		media_type_signature(DeclaredMediaType, '*', '*').

	media_type_signature(MediaType, Type, Subtype) :-
		atom(MediaType),
		atom_codes(MediaType, Codes),
		media_type_main_codes(Codes, MainCodes0),
		trim_ascii_space_codes(MainCodes0, MainCodes),
		append(TypeCodes0, [0'/| SubtypeCodes0], MainCodes),
		trim_ascii_space_codes(TypeCodes0, TypeCodes),
		trim_ascii_space_codes(SubtypeCodes0, SubtypeCodes),
		TypeCodes \== [],
		SubtypeCodes \== [],
		ascii_lower_codes(TypeCodes, LowerTypeCodes),
		ascii_lower_codes(SubtypeCodes, LowerSubtypeCodes),
		atom_codes(Type, LowerTypeCodes),
		atom_codes(Subtype, LowerSubtypeCodes).

	media_type_main_codes([], []).
	media_type_main_codes([0';| _], []) :-
		!.
	media_type_main_codes([Code| Codes], [Code| MainCodes]) :-
		media_type_main_codes(Codes, MainCodes).

	trim_ascii_space_codes(Codes, TrimmedCodes) :-
		trim_ascii_leading_space_codes(Codes, TrimmedLeadingCodes),
		reverse(TrimmedLeadingCodes, ReversedCodes),
		trim_ascii_leading_space_codes(ReversedCodes, ReversedTrimmedCodes),
		reverse(ReversedTrimmedCodes, TrimmedCodes).

	trim_ascii_leading_space_codes([Code| Codes], TrimmedCodes) :-
		ascii_space_code(Code),
		!,
		trim_ascii_leading_space_codes(Codes, TrimmedCodes).
	trim_ascii_leading_space_codes(Codes, Codes).

	ascii_space_code(0' ).
	ascii_space_code(0'\t).

	resolve_schema_for_validation(Provider, Schema, ResolvedSchema) :-
		resolve_schema_for_validation(Provider, Schema, [], ResolvedSchema).

	resolve_schema_for_validation(Provider, schema_ref(Name), Seen, ResolvedSchema) :-
		!,
		(	member(Name, Seen) ->
			domain_error(open_api_component_schema, recursive(Name))
		;	Provider::schema(Name, Schema) ->
			resolve_schema_for_validation(Provider, Schema, [Name| Seen], ResolvedSchema)
		;	existence_error(open_api_component_schema, Name)
		).
	resolve_schema_for_validation(_, [], _, []) :-
		!.
	resolve_schema_for_validation(Provider, [Schema| Schemas], Seen, [ResolvedSchema| ResolvedSchemas]) :-
		!,
		resolve_schema_for_validation(Provider, Schema, Seen, ResolvedSchema),
		resolve_schema_for_validation(Provider, Schemas, Seen, ResolvedSchemas).
	resolve_schema_for_validation(Provider, Schema, Seen, ResolvedSchema) :-
		^^json_object_pairs(Schema, Pairs),
		!,
		resolve_schema_pairs_for_validation(Pairs, Provider, Seen, ResolvedPairs),
		^^pairs_to_object(ResolvedPairs, ResolvedSchema).
	resolve_schema_for_validation(_, Schema, _, Schema).

	resolve_schema_pairs_for_validation([], _, _, []).
	resolve_schema_pairs_for_validation([Pair| Pairs], Provider, Seen, [Key-ResolvedValue| ResolvedPairs]) :-
		^^pair_key_value(Pair, Key, Value),
		resolve_schema_for_validation(Provider, Value, Seen, ResolvedValue),
		resolve_schema_pairs_for_validation(Pairs, Provider, Seen, ResolvedPairs).

	payload_json_value(json(Value), JsonValue) :-
		!,
		^^normalize_json_value(Value, JsonValue).
	payload_json_value(text(Text), JsonValue) :-
		!,
		^^normalize_json_value(Text, JsonValue).
	payload_json_value(binary(Bytes), JsonValue) :-
		!,
		^^normalize_json_value(Bytes, JsonValue).
	payload_json_value(form(Pairs), JsonValue) :-
		!,
		^^normalize_json_pairs(Pairs, JsonPairs),
		^^pairs_to_object(JsonPairs, JsonValue).

	payload_type(Payload, Type) :-
		compound(Payload),
		!,
		functor(Payload, Type, _).
	payload_type(Payload, Payload).

	request_target_path(origin(Path), Path) :-
		!.
	request_target_path(origin(Path, _), Path) :-
		!.
	request_target_path(absolute(Components), Path) :-
		!,
		absolute_target_path(Components, Path).

	absolute_target_path([path(Path)| _], Path) :-
		!.
	absolute_target_path([_| Components], Path) :-
		absolute_target_path(Components, Path).

	path_template_parameter_pairs(TemplatePath, ActualPath, ParameterPairs) :-
		path_segments(TemplatePath, TemplateSegments),
		path_segments(ActualPath, ActualSegments),
		path_segments_parameter_pairs_(TemplateSegments, ActualSegments, ParameterPairs).

	path_template_parameter_names(Path, Names) :-
		path_segments(Path, Segments),
		path_segments_template_parameter_names(Segments, [], ReversedNames),
		reverse(ReversedNames, Names).

	path_template_signature(Path, Signature) :-
		path_segments(Path, Segments),
		path_segments_template_signature(Segments, [], ReversedSignature),
		reverse(ReversedSignature, Signature).

	path_segments_template_parameter_names([], Names, Names).
	path_segments_template_parameter_names([Segment| Segments], Names0, Names) :-
		(	template_parameter_name(Segment, Name),
			\+ memberchk(Name, Names0) ->
			Names1 = [Name| Names0]
		;	Names1 = Names0
		),
		path_segments_template_parameter_names(Segments, Names1, Names).

	path_segments_template_signature([], Signature, Signature).
	path_segments_template_signature([Segment| Segments], Signature0, Signature) :-
		(	template_parameter_name(Segment, _) ->
			NormalizedSegment = '{}'
		;	NormalizedSegment = Segment
		),
		path_segments_template_signature(Segments, [NormalizedSegment| Signature0], Signature).

	path_segments_parameter_pairs_([], [], []).
	path_segments_parameter_pairs_([TemplateSegment| TemplateSegments], [ActualSegment| ActualSegments], Pairs) :-
		(	template_parameter_name(TemplateSegment, Name) ->
			Pairs = [Name-ActualSegment| Pairs0]
		;	TemplateSegment == ActualSegment,
			Pairs = Pairs0
		),
		path_segments_parameter_pairs_(TemplateSegments, ActualSegments, Pairs0).

	template_parameter_name(Segment, Name) :-
		atom_codes(Segment, [0'{| RestCodes]),
		append(NameCodes, [0'}], RestCodes),
		NameCodes \== [],
		atom_codes(Name, NameCodes).

	path_segments(Path, Segments) :-
		atom(Path),
		!,
		atom_codes(Path, Codes),
		split_path_codes(Codes, [], [], ReversedSegments),
		reverse(ReversedSegments, Segments).

	split_path_codes([], CurrentCodes, Acc, [Segment| Acc]) :-
		reverse(CurrentCodes, SegmentCodes),
		atom_codes(Segment, SegmentCodes).
	split_path_codes([0'/| Codes], CurrentCodes, Acc, Segments) :-
		!,
		reverse(CurrentCodes, SegmentCodes),
		atom_codes(Segment, SegmentCodes),
		split_path_codes(Codes, [], [Segment| Acc], Segments).
	split_path_codes([Code| Codes], CurrentCodes, Acc, Segments) :-
		split_path_codes(Codes, [Code| CurrentCodes], Acc, Segments).

	reconcile_path_parameters(DerivedPathParameters, [], DerivedPathParameters, []) :-
		!.
	reconcile_path_parameters(DerivedPathParameters, ProvidedPathParameters, ProvidedPathParameters, Errors) :-
		validate_declared_path_parameters(DerivedPathParameters, ProvidedPathParameters, Errors).

	validate_declared_path_parameters([], _, []).
	validate_declared_path_parameters([Name-Value| Parameters], DeclaredParameters, Errors) :-
		(	lookup_pair_value(Name, DeclaredParameters, DeclaredValue) ->
			(	Value == DeclaredValue ->
				CurrentErrors = []
			;	CurrentErrors = [path_parameter_mismatch(Name, Value, DeclaredValue)]
			)
		;	CurrentErrors = [missing_parameter(path, Name)]
		),
		validate_declared_path_parameters(Parameters, DeclaredParameters, RestErrors),
		append(CurrentErrors, RestErrors, Errors).

	path_parameters_property(Properties, Pairs) :-
		(	property_list_value(path_params, Properties, Pairs) ->
			true
		;	Pairs = []
		).

	query_pairs_property(Properties, Pairs) :-
		(	property_list_value(query_pairs, Properties, Pairs) ->
			true
		;	Pairs = []
		).

	cookie_pairs_property(Properties, Pairs) :-
		(	property_list_value(cookies, Properties, Pairs) ->
			true
		;	Pairs = []
		).

	property_list_value(_, Properties, _) :-
		var(Properties),
		!,
		fail.
	property_list_value(Name, [Property| _], Value) :-
		nonvar(Property),
		functor(Property, Name, 1),
		arg(1, Property, Value),
		!.
	property_list_value(Name, [_| Properties], Value) :-
		property_list_value(Name, Properties, Value).

	lookup_pair_value(_, Pairs, _) :-
		var(Pairs),
		!,
		fail.
	lookup_pair_value(Key, [Pair| _], Value) :-
		^^pair_key_value(Pair, Key, Value),
		!.
	lookup_pair_value(Key, [_| Pairs], Value) :-
		lookup_pair_value(Key, Pairs, Value).

	% provider readers

	provider_info_object(Provider, Info) :-
		Provider::api_info(Descriptor),
		api_info_descriptor_to_json(Descriptor, BaseInfo),
		merge_application_info(Provider, BaseInfo, Info).

	provider_servers_array(Provider, Servers) :-
		Provider::servers(Descriptors),
		server_descriptors_to_json(Descriptors, Servers).

	provider_security_pairs(Provider, [security-JsonRequirements]) :-
		catch(Provider::security(Requirements), _, fail),
		provider_security_scheme_pairs(Provider, SecuritySchemePairs, _),
		validate_document_security_requirements(Requirements, SecuritySchemePairs),
		security_requirements_to_json(Requirements, JsonRequirements),
		!.
	provider_security_pairs(_, []).

	provider_paths_object(Provider, Paths) :-
		Provider::operations(Descriptors),
		check_unique_operation_ids(Descriptors),
		validate_operation_paths(Descriptors),
		provider_security_scheme_pairs(Provider, SecuritySchemePairs, _),
		validate_operation_security_references(Descriptors, SecuritySchemePairs),
		validate_operation_path_parameters(Descriptors),
		operation_descriptors_to_paths_json(Descriptors, Paths).

	validate_operation_paths(Operations) :-
		validate_operation_paths(Operations, []).

	validate_operation_paths([], _).
	validate_operation_paths([operation(_, _, Path, _, _, _, _, _)| Operations], SeenSignatures) :-
		validate_operation_path(Path, SeenSignatures, SeenSignatures1),
		validate_operation_paths(Operations, SeenSignatures1).

	validate_operation_path(Path, SeenSignatures, SeenSignatures) :-
		(	\+ atom(Path)
		;	\+ sub_atom(Path, 0, 1, _, '/')
		),
		!,
		domain_error(open_api_path, invalid(Path)).
	validate_operation_path(Path, SeenSignatures, SeenSignatures) :-
		path_template_signature(Path, Signature),
		memberchk(Signature-SeenPath, SeenSignatures),
		SeenPath \== Path,
		!,
		domain_error(open_api_path(Path), duplicate_template(SeenPath)).
	validate_operation_path(Path, SeenSignatures, [Signature-Path| SeenSignatures]) :-
		path_template_signature(Path, Signature).

	check_unique_operation_ids(Operations) :-
		check_unique_operation_ids(Operations, []).

	check_unique_operation_ids([], _).
	check_unique_operation_ids([operation(Id, _, _, _, _, _, _, _)| Operations], SeenIds) :-
		(	member(Id, SeenIds) ->
			domain_error(open_api_operation_id, duplicate(Id))
		;	check_unique_operation_ids(Operations, [Id| SeenIds])
		).

	validate_document_security_requirements([], _) :-
		!.
	validate_document_security_requirements([Requirement| Requirements], SecuritySchemePairs) :-
		!,
		validate_security_requirement_references(Requirement, SecuritySchemePairs),
		validate_document_security_requirements(Requirements, SecuritySchemePairs).
	validate_document_security_requirements(Requirements, _) :-
		domain_error(open_api_security, Requirements).

	validate_operation_security_references([], _).
	validate_operation_security_references([operation(_, _, _, _, _, _, _, Properties)| Operations], SecuritySchemePairs) :-
		validate_operation_properties_security_references(Properties, SecuritySchemePairs),
		validate_operation_security_references(Operations, SecuritySchemePairs).

	validate_operation_properties_security_references([], _).
	validate_operation_properties_security_references([security(Requirements)| _], SecuritySchemePairs) :-
		!,
		validate_operation_security_requirements(Requirements, SecuritySchemePairs).
	validate_operation_properties_security_references([_| Properties], SecuritySchemePairs) :-
		validate_operation_properties_security_references(Properties, SecuritySchemePairs).

	validate_operation_path_parameters([]).
	validate_operation_path_parameters([operation(_, Method, Path, _, Parameters, _, _, _)| Operations]) :-
		path_template_parameter_names(Path, TemplateNames),
		validate_operation_parameter_definitions(Parameters, Path, Method, TemplateNames, [], [], DeclaredNames),
		validate_operation_path_parameter_coverage(TemplateNames, DeclaredNames, Path, Method),
		validate_operation_path_parameters(Operations).

	validate_operation_parameter_definitions([], _, _, _, _, DeclaredNames, DeclaredNames).
	validate_operation_parameter_definitions([parameter(Name, In, _Description, Required, _Schema)| Parameters], Path, Method, TemplateNames, Seen0, DeclaredNames0, DeclaredNames) :-
		(	ignored_parameter(Name, In) ->
			Seen1 = Seen0,
			DeclaredNames1 = DeclaredNames0
		;	validate_operation_parameter_uniqueness(Path, Method, Name, In, Seen0, Seen1),
			(	In == path ->
				validate_operation_path_parameter_required(Path, Method, Name, Required),
				validate_operation_path_parameter_name(Path, Method, Name, TemplateNames),
				DeclaredNames1 = [Name| DeclaredNames0]
			;	DeclaredNames1 = DeclaredNames0
			)
		),
		validate_operation_parameter_definitions(Parameters, Path, Method, TemplateNames, Seen1, DeclaredNames1, DeclaredNames).
	validate_operation_parameter_definitions([_| Parameters], Path, Method, TemplateNames, Seen0, DeclaredNames0, DeclaredNames) :-
		validate_operation_parameter_definitions(Parameters, Path, Method, TemplateNames, Seen0, DeclaredNames0, DeclaredNames).

	validate_operation_parameter_uniqueness(Path, Method, Name, In, Seen, Seen) :-
		memberchk(Name-In, Seen),
		!,
		(	In == path ->
			domain_error(open_api_path_parameter(Path, Method, Name), duplicate)
		;	domain_error(open_api_parameter(Path, Method, Name, In), duplicate)
		).
	validate_operation_parameter_uniqueness(_, _, Name, In, Seen, [Name-In| Seen]).

	validate_operation_path_parameter_required(_, _, _, true) :-
		!.
	validate_operation_path_parameter_required(Path, Method, Name, _) :-
		domain_error(open_api_path_parameter(Path, Method, Name), missing_required(true)).

	validate_operation_path_parameter_name(_, _, Name, TemplateNames) :-
		memberchk(Name, TemplateNames),
		!.
	validate_operation_path_parameter_name(Path, Method, Name, _) :-
		domain_error(open_api_path_parameter(Path, Method, Name), not_in_path_template).

	validate_operation_path_parameter_coverage([], _, _, _).
	validate_operation_path_parameter_coverage([Name| Names], DeclaredNames, Path, Method) :-
		(	memberchk(Name, DeclaredNames) ->
			true
		;	domain_error(open_api_path_parameter(Path, Method, Name), missing_from_operation_parameters)
		),
		validate_operation_path_parameter_coverage(Names, DeclaredNames, Path, Method).

	validate_operation_security_requirements([], _) :-
		!.
	validate_operation_security_requirements([Requirement| Requirements], SecuritySchemePairs) :-
		!,
		validate_security_requirement_references(Requirement, SecuritySchemePairs),
		validate_operation_security_requirements(Requirements, SecuritySchemePairs).
	validate_operation_security_requirements(Requirements, _) :-
		domain_error(open_api_operation_property, security(Requirements)).

	validate_security_requirement_references([], _) :-
		!.
	validate_security_requirement_references([Scheme-Scopes| Requirements], SecuritySchemePairs) :-
		!,
		(	lookup_pair_value(Scheme, SecuritySchemePairs, SecurityScheme) ->
			validate_security_requirement_scopes(Scheme, SecurityScheme, Scopes),
			validate_security_requirement_references(Requirements, SecuritySchemePairs)
		;	existence_error(open_api_security_scheme, Scheme)
		).
	validate_security_requirement_references([Requirement| _], _) :-
		domain_error(open_api_security_requirement, Requirement).

	validate_security_requirement_scopes(SchemeName, SecurityScheme, Scopes) :-
		security_requirement_scopes_to_json(Scopes, _),
		(	security_scheme_declared_scope_names(SecurityScheme, DeclaredScopes) ->
			validate_security_scope_names(Scopes, SchemeName, DeclaredScopes)
		;	true
		).

	validate_security_scope_names([], _, _).
	validate_security_scope_names([Scope| Scopes], SchemeName, DeclaredScopes) :-
		(	member(Scope, DeclaredScopes) ->
			validate_security_scope_names(Scopes, SchemeName, DeclaredScopes)
		;	existence_error(open_api_security_scope(SchemeName), Scope)
		).

	security_scheme_declared_scope_names(SecurityScheme, ScopeNames) :-
		security_scheme_type(SecurityScheme, Type),
		(	Type == oauth2 ; Type == openIdConnect ),
		security_scheme_flows_object(SecurityScheme, Flows),
		oauth_flows_scope_names(Flows, [], ScopeNames).

	security_scheme_type(SecurityScheme, Type) :-
		^^json_object_pairs(SecurityScheme, Pairs),
		lookup_pair_value(type, Pairs, Type).

	security_scheme_flows_object(SecurityScheme, Flows) :-
		^^json_object_pairs(SecurityScheme, Pairs),
		lookup_pair_value(flows, Pairs, Flows).

	oauth_flows_scope_names(Flows, Acc, ScopeNames) :-
		^^json_object_pairs(Flows, FlowPairs),
		oauth_flow_pairs_scope_names(FlowPairs, Acc, ScopeNames).

	oauth_flow_pairs_scope_names([], ScopeNames, ScopeNames).
	oauth_flow_pairs_scope_names([_FlowName-FlowObject| FlowPairs], Acc, ScopeNames) :-
		oauth_flow_scope_names(FlowObject, Acc, Acc1),
		oauth_flow_pairs_scope_names(FlowPairs, Acc1, ScopeNames).

	oauth_flow_scope_names(FlowObject, Acc, ScopeNames) :-
		(	^^json_object_pairs(FlowObject, FlowPairs),
			lookup_pair_value(scopes, FlowPairs, ScopesObject) ->
			oauth_scope_object_names(ScopesObject, Acc, ScopeNames)
		;	ScopeNames = Acc
		).

	oauth_scope_object_names(ScopesObject, Acc, ScopeNames) :-
		^^json_object_pairs(ScopesObject, ScopePairs),
		oauth_scope_pairs_names(ScopePairs, Acc, ScopeNames).

	oauth_scope_pairs_names([], ScopeNames, ScopeNames).
	oauth_scope_pairs_names([Pair| ScopePairs], Acc, ScopeNames) :-
		^^pair_key_value(Pair, ScopeName, _Description),
		(	member(ScopeName, Acc) ->
			Acc1 = Acc
		;	Acc1 = [ScopeName| Acc]
		),
		oauth_scope_pairs_names(ScopePairs, Acc1, ScopeNames).

	provider_components_object(Provider, Components) :-
		provider_schemas_object(Provider, Schemas),
		provider_security_schemes_object(Provider, SecuritySchemes),
		append_security_schemes_pair(SecuritySchemes, [schemas-Schemas], Pairs),
		^^pairs_to_object(Pairs, Components).

	append_security_schemes_pair({}, Pairs, Pairs) :-
		!.
	append_security_schemes_pair(SecuritySchemes, Pairs0, Pairs) :-
		append(Pairs0, [securitySchemes-SecuritySchemes], Pairs).

	provider_external_docs_pairs(Provider, [externalDocs-{url-Homepage}]) :-
		conforms_to_protocol(Provider, application_protocol),
		catch(Provider::homepage(Homepage), _, fail),
		validate_provider_url_reference(open_api_external_docs, Homepage),
		!.
	provider_external_docs_pairs(_, []).

	% info mapping

	api_info_descriptor_to_json(info(Title, Version, Summary, Properties), Info) :-
		info_properties_to_pairs(Properties, PropertyPairs),
		^^pairs_to_object([title-Title, version-Version, summary-Summary| PropertyPairs], Info).

	info_properties_to_pairs(Properties, Pairs) :-
		info_properties_to_pairs(Properties, no, Pairs).

	info_properties_to_pairs([], _, []).
	info_properties_to_pairs([description(Description)| Properties], no, [description-Description| Pairs0]) :-
		!,
		info_properties_to_pairs(Properties, yes, Pairs0).
	info_properties_to_pairs([description(_)| _], yes, _) :-
		!,
		domain_error(open_api_info_property, duplicate(description)).
	info_properties_to_pairs([Property| _], _, _) :-
		domain_error(open_api_info_property, Property).

	merge_application_info(Provider, BaseInfo, Info) :-
		(	conforms_to_protocol(Provider, application_protocol) ->
			^^json_object_pairs(BaseInfo, BasePairs),
			merge_application_description(Provider, BasePairs, Pairs1),
			merge_application_license(Provider, Pairs1, Pairs2),
			^^pairs_to_object(Pairs2, Info)
		;	Info = BaseInfo
		).

	merge_application_description(_, Pairs, Pairs) :-
		member(description-_, Pairs),
		!.
	merge_application_description(Provider, Pairs, MergedPairs) :-
		(	catch(Provider::description(Description), _, fail) ->
			append(Pairs, [description-Description], MergedPairs)
		;	MergedPairs = Pairs
		).

	merge_application_license(_, Pairs, Pairs) :-
		member(license-_, Pairs),
		!.
	merge_application_license(Provider, Pairs, MergedPairs) :-
		(	catch(Provider::license(License), _, fail) ->
			append(Pairs, [license-{name-License}], MergedPairs)
		;	MergedPairs = Pairs
		).

	% servers mapping

	server_descriptors_to_json([], []).
	server_descriptors_to_json([Descriptor| Descriptors], [JsonServer| JsonServers]) :-
		server_descriptor_to_json(Descriptor, JsonServer),
		server_descriptors_to_json(Descriptors, JsonServers).

	server_descriptor_to_json(server(URL, Description), {url-URL, description-Description}) :-
		validate_server_url(URL).

	% path mapping and grouping

	operation_descriptors_to_paths_json(Descriptors, Paths) :-
		group_operations_by_path(Descriptors, Grouped),
		grouped_paths_to_pairs(Grouped, PathPairs),
		^^pairs_to_object(PathPairs, Paths).

	group_operations_by_path(Operations, Grouped) :-
		group_operations_by_path(Operations, [], Grouped).

	group_operations_by_path([], Grouped, Grouped).
	group_operations_by_path([Operation| Operations], Grouped0, Grouped) :-
		add_operation_to_path_group(Operation, Grouped0, Grouped1),
		group_operations_by_path(Operations, Grouped1, Grouped).

	add_operation_to_path_group(Operation, Grouped0, Grouped) :-
		Operation = operation(_, _, Path, _, _, _, _, _),
		add_operation_to_path_group(Grouped0, Path, Operation, [], Grouped).

	add_operation_to_path_group([], Path, Operation, PrefixRev, Grouped) :-
		reverse(PrefixRev, Prefix),
		append(Prefix, [Path-[Operation]], Grouped).
	add_operation_to_path_group([Path-Operations| Rest], Path, Operation, PrefixRev, Grouped) :-
		!,
		append(Operations, [Operation], UpdatedOperations),
		reverse(PrefixRev, Prefix),
		append(Prefix, [Path-UpdatedOperations| Rest], Grouped).
	add_operation_to_path_group([Group| Rest], Path, Operation, PrefixRev, Grouped) :-
		add_operation_to_path_group(Rest, Path, Operation, [Group| PrefixRev], Grouped).

	grouped_paths_to_pairs([], []).
	grouped_paths_to_pairs([Path-Operations| Grouped], [Path-PathObject| Pairs]) :-
		path_operations_to_json(Operations, PathObject),
		grouped_paths_to_pairs(Grouped, Pairs).

	path_operations_to_json([], {}) :-
		!.
	path_operations_to_json(Operations, PathObject) :-
		path_operations_to_json(Operations, [], Pairs),
		^^pairs_to_object(Pairs, PathObject).

	path_operations_to_json([], _, []).
	path_operations_to_json([Operation| Operations], SeenMethods, [Method-OperationObject| Pairs]) :-
		Operation = operation(_, Method, Path, _, _, _, _, _),
		(	member(Method, SeenMethods) ->
			domain_error(open_api_path_method(Path), duplicate(Method))
		;	operation_descriptor_to_json(Operation, OperationObject),
			path_operations_to_json(Operations, [Method| SeenMethods], Pairs)
		).

	operation_descriptor_to_json(
		operation(Id, _Method, _Path, Summary, Parameters, RequestBody, Responses, Properties),
		Operation
	) :-
		parameters_to_json(Parameters, JsonParameters),
		responses_to_json(Responses, JsonResponses),
		operation_properties_to_pairs(Properties, PropertyPairs),
		Pairs0 = [operationId-Id, summary-Summary| PropertyPairs],
		append(Pairs0, [parameters-JsonParameters], Pairs1),
		append_request_body_pair(RequestBody, Pairs1, Pairs2),
		append(Pairs2, [responses-JsonResponses], Pairs3),
		^^pairs_to_object(Pairs3, Operation).

	operation_properties_to_pairs(Properties, Pairs) :-
		operation_properties_to_pairs(Properties, no, no, no, no, Pairs).

	operation_properties_to_pairs([], _, _, _, _, []).
	operation_properties_to_pairs([description(Description)| Properties], no, TagsSeen, DeprecatedSeen, SecuritySeen, [description-Description| Pairs]) :-
		!,
		operation_properties_to_pairs(Properties, yes, TagsSeen, DeprecatedSeen, SecuritySeen, Pairs).
	operation_properties_to_pairs([description(_)| _], yes, _, _, _, _) :-
		!,
		domain_error(open_api_operation_property, duplicate(description)).
	operation_properties_to_pairs([tags(Tags)| Properties], DescriptionSeen, no, DeprecatedSeen, SecuritySeen, [tags-Tags| Pairs]) :-
		!,
		operation_properties_to_pairs(Properties, DescriptionSeen, yes, DeprecatedSeen, SecuritySeen, Pairs).
	operation_properties_to_pairs([tags(_)| _], _, yes, _, _, _) :-
		!,
		domain_error(open_api_operation_property, duplicate(tags)).
	operation_properties_to_pairs([deprecated(Deprecated)| Properties], DescriptionSeen, TagsSeen, no, SecuritySeen, [Pair| Pairs]) :-
		!,
		deprecated_property_pair(Deprecated, Pair),
		operation_properties_to_pairs(Properties, DescriptionSeen, TagsSeen, yes, SecuritySeen, Pairs).
	operation_properties_to_pairs([deprecated(_)| _], _, _, yes, _, _) :-
		!,
		domain_error(open_api_operation_property, duplicate(deprecated)).
	operation_properties_to_pairs([security(Requirements)| Properties], DescriptionSeen, TagsSeen, DeprecatedSeen, no, [Pair| Pairs]) :-
		!,
		security_property_pair(Requirements, Pair),
		operation_properties_to_pairs(Properties, DescriptionSeen, TagsSeen, DeprecatedSeen, yes, Pairs).
	operation_properties_to_pairs([security(_)| _], _, _, _, yes, _) :-
		!,
		domain_error(open_api_operation_property, duplicate(security)).
	operation_properties_to_pairs([Property| _], _, _, _, _, _) :-
		domain_error(open_api_operation_property, Property).

	deprecated_property_pair(Deprecated, deprecated-JsonDeprecated) :-
		(	bool_to_json(Deprecated, JsonDeprecated) ->
			true
		;	domain_error(open_api_operation_property, deprecated(Deprecated))
		).

	security_property_pair(Requirements, security-JsonRequirements) :-
		security_requirements_to_json(Requirements, JsonRequirements).

	security_requirements_to_json([], []) :-
		!.
	security_requirements_to_json([Requirement| Requirements], [JsonRequirement| JsonRequirements]) :-
		!,
		security_requirement_to_json(Requirement, JsonRequirement),
		security_requirements_to_json(Requirements, JsonRequirements).
	security_requirements_to_json(Requirements, _) :-
		domain_error(open_api_operation_property, security(Requirements)).

	security_requirement_to_json([], {}) :-
		!.
	security_requirement_to_json(Requirement, JsonRequirement) :-
		security_requirement_pairs_to_json(Requirement, [], Pairs),
		^^pairs_to_object(Pairs, JsonRequirement).

	security_requirement_pairs_to_json([], _, []).
	security_requirement_pairs_to_json([Scheme-Scopes| Requirements], SeenSchemes, [Scheme-JsonScopes| Pairs]) :-
		!,
		(	member(Scheme, SeenSchemes) ->
			domain_error(open_api_security_requirement, duplicate(Scheme))
		;	security_requirement_scopes_to_json(Scopes, JsonScopes),
			security_requirement_pairs_to_json(Requirements, [Scheme| SeenSchemes], Pairs)
		).
	security_requirement_pairs_to_json([Requirement| _], _, _) :-
		domain_error(open_api_security_requirement, Requirement).

	security_requirement_scopes_to_json([], []) :-
		!.
	security_requirement_scopes_to_json([Scope| Scopes], [Scope| JsonScopes]) :-
		!,
		security_requirement_scopes_to_json(Scopes, JsonScopes).
	security_requirement_scopes_to_json(Scopes, _) :-
		domain_error(open_api_security_requirement_scopes, Scopes).

	append_request_body_pair(none, Pairs, Pairs) :-
		!.
	append_request_body_pair(RequestBody, Pairs0, Pairs) :-
		request_body_to_json(RequestBody, JsonRequestBody),
		append(Pairs0, [requestBody-JsonRequestBody], Pairs).

	parameters_to_json([], []).
	parameters_to_json([parameter(Name, In, _Description, _Required, _Schema)| Descriptors], JsonParameters) :-
		ignored_parameter(Name, In),
		!,
		parameters_to_json(Descriptors, JsonParameters).
	parameters_to_json([Descriptor| Descriptors], [JsonParameter| JsonParameters]) :-
		parameter_descriptor_to_json(Descriptor, JsonParameter),
		parameters_to_json(Descriptors, JsonParameters).

	parameter_descriptor_to_json(parameter(Name, In, Description, Required, Schema), Parameter) :-
		bool_to_json(Required, JsonRequired),
		schema_term_to_json(Schema, JsonSchema),
		Parameter = {name-Name, in-In, description-Description, required-JsonRequired, schema-JsonSchema}.

	request_body_to_json(request_body(Description, Required, []), _) :-
		!,
		domain_error(open_api_request_body, request_body(Description, Required, [])).
	request_body_to_json(request_body(Description, Required, MediaTypes), RequestBody) :-
		bool_to_json(Required, JsonRequired),
		media_types_to_json(MediaTypes, Content),
		RequestBody = {description-Description, required-JsonRequired, content-Content}.

	responses_to_json([], _) :-
		!,
		domain_error(open_api_responses, []).
	responses_to_json(Descriptors, Responses) :-
		responses_to_json(Descriptors, [], Pairs),
		^^pairs_to_object(Pairs, Responses).

	responses_to_json([], _, []).
	responses_to_json([Descriptor| Descriptors], SeenKeys, [Key-JsonResponse| Pairs]) :-
		response_descriptor_to_pair(Descriptor, Key-JsonResponse),
		(	member(Key, SeenKeys) ->
			domain_error(open_api_response_status, duplicate(Key))
		;	responses_to_json(Descriptors, [Key| SeenKeys], Pairs)
		).

	response_descriptor_to_pair(response(Status, Description, []), Key-Response) :-
		!,
		status_key(Status, Key),
		Response = {description-Description}.
	response_descriptor_to_pair(response(Status, Description, MediaTypes), Key-Response) :-
		status_key(Status, Key),
		media_types_to_json(MediaTypes, Content),
		Response = {description-Description, content-Content}.

	media_types_to_json(Descriptors, Content) :-
		media_types_to_json(Descriptors, [], Pairs),
		^^pairs_to_object(Pairs, Content).

	media_types_to_json([], _, []).
	media_types_to_json([Descriptor| Descriptors], SeenTypes, [MediaType-JsonMedia| Pairs]) :-
		media_descriptor_to_pair(Descriptor, MediaType-JsonMedia),
		(	member(MediaType, SeenTypes) ->
			domain_error(open_api_media_type, duplicate(MediaType))
		;	media_types_to_json(Descriptors, [MediaType| SeenTypes], Pairs)
		).

	media_descriptor_to_pair(media(MediaType, Schema), MediaType-{schema-JsonSchema}) :-
		schema_term_to_json(Schema, JsonSchema).

	% components and schema mapping

	provider_schemas_object(Provider, Schemas) :-
		findall(Name-SchemaTerm, Provider::schema(Name, SchemaTerm), Descriptors),
		schema_descriptors_to_pairs(Descriptors, Pairs),
		^^pairs_to_object(Pairs, Schemas).

	provider_security_schemes_object(Provider, SecuritySchemes) :-
		provider_security_scheme_pairs(Provider, Pairs, _),
		^^pairs_to_object(Pairs, SecuritySchemes).

	provider_security_scheme_pairs(Provider, Pairs, Names) :-
		findall(Name-SecuritySchemeTerm, Provider::security_scheme(Name, SecuritySchemeTerm), Descriptors),
		security_scheme_descriptors_to_pairs(Descriptors, Pairs, Names).

	schema_descriptors_to_pairs(Descriptors, Pairs) :-
		schema_descriptors_to_pairs(Descriptors, [], Pairs).

	schema_descriptors_to_pairs([], _, []).
	schema_descriptors_to_pairs([Name-SchemaTerm| Descriptors], SeenNames, [Pair| Pairs]) :-
		(	member(Name, SeenNames) ->
			domain_error(open_api_component_schema, duplicate(Name))
		;	validate_component_member_name(open_api_component_schema, Name),
			schema_descriptor_to_pair(Name, SchemaTerm, Pair),
			schema_descriptors_to_pairs(Descriptors, [Name| SeenNames], Pairs)
		).

	schema_descriptor_to_pair(Name, SchemaTerm, Name-JsonSchema) :-
		schema_term_to_json(SchemaTerm, JsonSchema).

	security_scheme_descriptors_to_pairs(Descriptors, Pairs, Names) :-
		security_scheme_descriptors_to_pairs_(Descriptors, [], Pairs),
		security_scheme_descriptor_names(Pairs, Names).

	security_scheme_descriptors_to_pairs_([], _, []).
	security_scheme_descriptors_to_pairs_([Name-SecuritySchemeTerm| Descriptors], SeenNames, [Pair| Pairs]) :-
		(	member(Name, SeenNames) ->
			domain_error(open_api_security_scheme, duplicate(Name))
		;	validate_component_member_name(open_api_security_scheme, Name),
			security_scheme_descriptor_to_pair(Name, SecuritySchemeTerm, Pair),
			security_scheme_descriptors_to_pairs_(Descriptors, [Name| SeenNames], Pairs)
		).

	security_scheme_descriptor_names([], []).
	security_scheme_descriptor_names([Name-_| Pairs], [Name| Names]) :-
		security_scheme_descriptor_names(Pairs, Names).

	security_scheme_descriptor_to_pair(Name, SecuritySchemeTerm, Name-JsonSecurityScheme) :-
		security_scheme_descriptor_to_json(Name, SecuritySchemeTerm, JsonSecurityScheme),
		validate_security_scheme_object(Name, JsonSecurityScheme).

	security_scheme_descriptor_to_json(Name, SecuritySchemeTerm, JsonSecurityScheme) :-
		(	security_scheme_dsl_term(SecuritySchemeTerm) ->
			security_scheme_dsl_to_json(Name, SecuritySchemeTerm, JsonSecurityScheme)
		;	^^normalize_json_value(SecuritySchemeTerm, JsonSecurityScheme)
		).

	security_scheme_dsl_term(mutual_tls) :-
		!.
	security_scheme_dsl_term(SecuritySchemeTerm) :-
		compound(SecuritySchemeTerm),
		functor(SecuritySchemeTerm, Functor, _),
		security_scheme_dsl_functor(Functor).

	security_scheme_dsl_functor(api_key).
	security_scheme_dsl_functor(http).
	security_scheme_dsl_functor(mutual_tls).
	security_scheme_dsl_functor(oauth2).
	security_scheme_dsl_functor(openid_connect).

	security_scheme_dsl_to_json(Name, api_key(In, HeaderName), JsonSecurityScheme) :-
		!,
		security_scheme_api_key_pairs(Name, In, HeaderName, [], Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, api_key(In, HeaderName, Options), JsonSecurityScheme) :-
		!,
		security_scheme_api_key_pairs(Name, In, HeaderName, Options, Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, http(Scheme), JsonSecurityScheme) :-
		!,
		security_scheme_http_pairs(Name, Scheme, [], Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, http(Scheme, Options), JsonSecurityScheme) :-
		!,
		security_scheme_http_pairs(Name, Scheme, Options, Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, mutual_tls, JsonSecurityScheme) :-
		!,
		security_scheme_mutual_tls_pairs(Name, [], Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, mutual_tls(Options), JsonSecurityScheme) :-
		!,
		security_scheme_mutual_tls_pairs(Name, Options, Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, oauth2(FlowDescriptors), JsonSecurityScheme) :-
		!,
		security_scheme_oauth2_pairs(Name, FlowDescriptors, [], Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, oauth2(FlowDescriptors, Options), JsonSecurityScheme) :-
		!,
		security_scheme_oauth2_pairs(Name, FlowDescriptors, Options, Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, openid_connect(URL), JsonSecurityScheme) :-
		!,
		security_scheme_openid_connect_pairs(Name, URL, [], Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, openid_connect(URL, Options), JsonSecurityScheme) :-
		!,
		security_scheme_openid_connect_pairs(Name, URL, Options, Pairs),
		^^pairs_to_object(Pairs, JsonSecurityScheme).
	security_scheme_dsl_to_json(Name, SecuritySchemeTerm, _) :-
		domain_error(open_api_security_scheme(Name), invalid_descriptor(SecuritySchemeTerm)).

	security_scheme_api_key_pairs(Name, In, HeaderName, Options, Pairs) :-
		security_scheme_common_option_pairs(Name, apiKey, Options, OptionPairs),
		Pairs = [type-apiKey, name-HeaderName, in-In| OptionPairs].

	security_scheme_http_pairs(Name, Scheme, Options, Pairs) :-
		security_scheme_http_option_pairs(Name, Options, OptionPairs),
		Pairs = [type-http, scheme-Scheme| OptionPairs].

	security_scheme_mutual_tls_pairs(Name, Options, Pairs) :-
		security_scheme_common_option_pairs(Name, mutualTLS, Options, OptionPairs),
		Pairs = [type-mutualTLS| OptionPairs].

	security_scheme_oauth2_pairs(Name, FlowDescriptors, Options, Pairs) :-
		oauth_flow_descriptors_to_json(Name, oauth2, FlowDescriptors, JsonFlows),
		security_scheme_common_option_pairs(Name, oauth2, Options, OptionPairs),
		Pairs = [type-oauth2, flows-JsonFlows| OptionPairs].

	security_scheme_openid_connect_pairs(Name, URL, Options, Pairs) :-
		security_scheme_openid_connect_option_pairs(Name, Options, OptionPairs),
		Pairs = [type-openIdConnect, openIdConnectUrl-URL| OptionPairs].

	security_scheme_common_option_pairs(Name, Type, Options, Pairs) :-
		security_scheme_common_option_pairs(Options, Name, Type, no, Pairs).

	security_scheme_common_option_pairs([], _, _, _, []).
	security_scheme_common_option_pairs([description(Description)| Options], Name, Type, no, [description-Description| Pairs]) :-
		!,
		security_scheme_common_option_pairs(Options, Name, Type, yes, Pairs).
	security_scheme_common_option_pairs([description(_)| _], Name, Type, yes, _) :-
		!,
		domain_error(open_api_security_scheme(Name, Type), duplicate(description)).
	security_scheme_common_option_pairs([Option| _], Name, Type, _, _) :-
		!,
		domain_error(open_api_security_scheme(Name, Type), invalid_option(Option)).
	security_scheme_common_option_pairs(Options, Name, Type, _, _) :-
		domain_error(open_api_security_scheme(Name, Type), invalid_options(Options)).

	security_scheme_http_option_pairs(Name, Options, Pairs) :-
		security_scheme_http_option_pairs(Options, Name, no, no, Pairs).

	security_scheme_http_option_pairs([], _, _, _, []).
	security_scheme_http_option_pairs([description(Description)| Options], Name, no, BearerFormatSeen, [description-Description| Pairs]) :-
		!,
		security_scheme_http_option_pairs(Options, Name, yes, BearerFormatSeen, Pairs).
	security_scheme_http_option_pairs([description(_)| _], Name, yes, _, _) :-
		!,
		domain_error(open_api_security_scheme(Name, http), duplicate(description)).
	security_scheme_http_option_pairs([bearer_format(Format)| Options], Name, DescriptionSeen, no, [bearerFormat-Format| Pairs]) :-
		!,
		security_scheme_http_option_pairs(Options, Name, DescriptionSeen, yes, Pairs).
	security_scheme_http_option_pairs([bearer_format(_)| _], Name, _, yes, _) :-
		!,
		domain_error(open_api_security_scheme(Name, http), duplicate(bearer_format)).
	security_scheme_http_option_pairs([Option| _], Name, _, _, _) :-
		!,
		domain_error(open_api_security_scheme(Name, http), invalid_option(Option)).
	security_scheme_http_option_pairs(Options, Name, _, _, _) :-
		domain_error(open_api_security_scheme(Name, http), invalid_options(Options)).

	security_scheme_openid_connect_option_pairs(Name, Options, Pairs) :-
		security_scheme_openid_connect_option_pairs(Options, Name, no, no, Pairs).

	security_scheme_openid_connect_option_pairs([], _, _, _, []).
	security_scheme_openid_connect_option_pairs([description(Description)| Options], Name, no, _, [description-Description| Pairs]) :-
		!,
		security_scheme_openid_connect_option_pairs(Options, Name, yes, no, Pairs).
	security_scheme_openid_connect_option_pairs([description(_)| _], Name, yes, _, _) :-
		!,
		domain_error(open_api_security_scheme(Name, openIdConnect), duplicate(description)).
	security_scheme_openid_connect_option_pairs([Option| _], Name, _, _, _) :-
		!,
		domain_error(open_api_security_scheme(Name, openIdConnect), invalid_option(Option)).
	security_scheme_openid_connect_option_pairs(Options, Name, _, _, _) :-
		domain_error(open_api_security_scheme(Name, openIdConnect), invalid_options(Options)).

	oauth_flow_descriptors_to_json(Name, Type, FlowDescriptors, JsonFlows) :-
		oauth_flow_descriptors_to_pairs(FlowDescriptors, Name, Type, [], Pairs),
		^^pairs_to_object(Pairs, JsonFlows).

	oauth_flow_descriptors_to_pairs([], _, _, _, []).
	oauth_flow_descriptors_to_pairs([Descriptor| Descriptors], Name, Type, SeenFlowNames, [FlowName-JsonFlow| Pairs]) :-
		oauth_flow_descriptor_to_pair(Name, Type, Descriptor, FlowName-JsonFlow),
		(	member(FlowName, SeenFlowNames) ->
			domain_error(open_api_security_scheme(Name, Type), duplicate(oauth_flow(FlowName)))
		;	oauth_flow_descriptors_to_pairs(Descriptors, Name, Type, [FlowName| SeenFlowNames], Pairs)
		).
	oauth_flow_descriptors_to_pairs(FlowDescriptors, Name, Type, _, _) :-
		domain_error(open_api_security_scheme(Name, Type), invalid(flows, FlowDescriptors)).

	oauth_flow_descriptor_to_pair(Name, Type, implicit(AuthorizationURL, ScopeDescriptors), Pair) :-
		!,
		oauth_flow_descriptor_to_pair(Name, Type, implicit(AuthorizationURL, ScopeDescriptors, []), Pair).
	oauth_flow_descriptor_to_pair(Name, Type, implicit(AuthorizationURL, ScopeDescriptors, Options), implicit-JsonFlow) :-
		!,
		oauth_scope_descriptors_to_json(Name, Type, ScopeDescriptors, JsonScopes),
		oauth_flow_option_pairs(Name, Type, implicit, Options, OptionPairs),
		Pairs = [authorizationUrl-AuthorizationURL, scopes-JsonScopes| OptionPairs],
		^^pairs_to_object(Pairs, JsonFlow).
	oauth_flow_descriptor_to_pair(Name, Type, password(TokenURL, ScopeDescriptors), Pair) :-
		!,
		oauth_flow_descriptor_to_pair(Name, Type, password(TokenURL, ScopeDescriptors, []), Pair).
	oauth_flow_descriptor_to_pair(Name, Type, password(TokenURL, ScopeDescriptors, Options), password-JsonFlow) :-
		!,
		oauth_scope_descriptors_to_json(Name, Type, ScopeDescriptors, JsonScopes),
		oauth_flow_option_pairs(Name, Type, password, Options, OptionPairs),
		Pairs = [tokenUrl-TokenURL, scopes-JsonScopes| OptionPairs],
		^^pairs_to_object(Pairs, JsonFlow).
	oauth_flow_descriptor_to_pair(Name, Type, client_credentials(TokenURL, ScopeDescriptors), Pair) :-
		!,
		oauth_flow_descriptor_to_pair(Name, Type, client_credentials(TokenURL, ScopeDescriptors, []), Pair).
	oauth_flow_descriptor_to_pair(Name, Type, client_credentials(TokenURL, ScopeDescriptors, Options), clientCredentials-JsonFlow) :-
		!,
		oauth_scope_descriptors_to_json(Name, Type, ScopeDescriptors, JsonScopes),
		oauth_flow_option_pairs(Name, Type, clientCredentials, Options, OptionPairs),
		Pairs = [tokenUrl-TokenURL, scopes-JsonScopes| OptionPairs],
		^^pairs_to_object(Pairs, JsonFlow).
	oauth_flow_descriptor_to_pair(Name, Type, authorization_code(AuthorizationURL, TokenURL, ScopeDescriptors), Pair) :-
		!,
		oauth_flow_descriptor_to_pair(Name, Type, authorization_code(AuthorizationURL, TokenURL, ScopeDescriptors, []), Pair).
	oauth_flow_descriptor_to_pair(Name, Type, authorization_code(AuthorizationURL, TokenURL, ScopeDescriptors, Options), authorizationCode-JsonFlow) :-
		!,
		oauth_scope_descriptors_to_json(Name, Type, ScopeDescriptors, JsonScopes),
		oauth_flow_option_pairs(Name, Type, authorizationCode, Options, OptionPairs),
		Pairs = [authorizationUrl-AuthorizationURL, tokenUrl-TokenURL, scopes-JsonScopes| OptionPairs],
		^^pairs_to_object(Pairs, JsonFlow).
	oauth_flow_descriptor_to_pair(Name, Type, Descriptor, _) :-
		domain_error(open_api_security_scheme(Name, Type), invalid(oauth_flow_descriptor, Descriptor)).

	oauth_flow_option_pairs(Name, Type, FlowName, Options, Pairs) :-
		oauth_flow_option_pairs(Options, Name, Type, FlowName, no, Pairs).

	oauth_flow_option_pairs([], _, _, _, _, []).
	oauth_flow_option_pairs([refresh_url(URL)| Options], Name, Type, FlowName, no, [refreshUrl-URL| Pairs]) :-
		!,
		oauth_flow_option_pairs(Options, Name, Type, FlowName, yes, Pairs).
	oauth_flow_option_pairs([refresh_url(_)| _], Name, Type, FlowName, yes, _) :-
		!,
		domain_error(open_api_security_scheme(Name, Type), duplicate(oauth_flow_field(FlowName, refreshUrl))).
	oauth_flow_option_pairs([Option| _], Name, Type, FlowName, _, _) :-
		!,
		domain_error(open_api_security_scheme(Name, Type), invalid_option(oauth_flow(FlowName, Option))).
	oauth_flow_option_pairs(Options, Name, Type, FlowName, _, _) :-
		domain_error(open_api_security_scheme(Name, Type), invalid_options(oauth_flow(FlowName, Options))).

	oauth_scope_descriptors_to_json(Name, Type, ScopeDescriptors, JsonScopes) :-
		oauth_scope_descriptors_to_pairs(ScopeDescriptors, Name, Type, [], Pairs),
		^^pairs_to_object(Pairs, JsonScopes).

	oauth_scope_descriptors_to_pairs([], _, _, _, []).
	oauth_scope_descriptors_to_pairs([Descriptor| Descriptors], Name, Type, SeenScopeNames, [ScopeName-Description| Pairs]) :-
		oauth_scope_descriptor_to_pair(Name, Type, Descriptor, ScopeName-Description),
		(	member(ScopeName, SeenScopeNames) ->
			domain_error(open_api_security_scheme(Name, Type), duplicate(scope(ScopeName)))
		;	oauth_scope_descriptors_to_pairs(Descriptors, Name, Type, [ScopeName| SeenScopeNames], Pairs)
		).
	oauth_scope_descriptors_to_pairs(ScopeDescriptors, Name, Type, _, _) :-
		domain_error(open_api_security_scheme(Name, Type), invalid(scopes, ScopeDescriptors)).

	oauth_scope_descriptor_to_pair(_, _, scope(ScopeName, Description), ScopeName-Description) :-
		!.
	oauth_scope_descriptor_to_pair(Name, Type, ScopeDescriptor, _) :-
		domain_error(open_api_security_scheme(Name, Type), invalid(scope, ScopeDescriptor)).

	validate_security_scheme_object(Name, SecurityScheme) :-
		^^json_object_pairs(SecurityScheme, Pairs),
		!,
		validate_security_scheme_pairs(Name, Pairs).
	validate_security_scheme_object(Name, SecurityScheme) :-
		domain_error(open_api_security_scheme(Name), SecurityScheme).

	validate_security_scheme_pairs(Name, Pairs) :-
		security_scheme_type_from_pairs(Name, Pairs, Type),
		validate_security_scheme_type_properties(Name, Type, Pairs).

	security_scheme_type_from_pairs(Name, Pairs, Type) :-
		(	lookup_pair_value(type, Pairs, Type) ->
			validate_security_scheme_type(Name, Type)
		;	domain_error(open_api_security_scheme(Name), missing(type))
		).

	validate_security_scheme_type(_, apiKey) :-
		!.
	validate_security_scheme_type(_, http) :-
		!.
	validate_security_scheme_type(_, mutualTLS) :-
		!.
	validate_security_scheme_type(_, oauth2) :-
		!.
	validate_security_scheme_type(_, openIdConnect) :-
		!.
	validate_security_scheme_type(Name, Type) :-
		domain_error(open_api_security_scheme(Name), invalid_type(Type)).

	validate_security_scheme_type_properties(Name, apiKey, Pairs) :-
		require_security_scheme_field(Name, apiKey, Pairs, name, _),
		require_security_scheme_field(Name, apiKey, Pairs, in, In),
		validate_api_key_location(Name, In).
	validate_security_scheme_type_properties(Name, http, Pairs) :-
		require_security_scheme_field(Name, http, Pairs, scheme, _).
	validate_security_scheme_type_properties(_, mutualTLS, _).
	validate_security_scheme_type_properties(Name, oauth2, Pairs) :-
		require_security_scheme_field(Name, oauth2, Pairs, flows, Flows),
		validate_oauth_flows_object(Name, oauth2, Flows).
	validate_security_scheme_type_properties(Name, openIdConnect, Pairs) :-
		require_security_scheme_field(Name, openIdConnect, Pairs, openIdConnectUrl, OpenIdConnectUrl),
		validate_security_scheme_url_field(Name, openIdConnect, openIdConnectUrl, OpenIdConnectUrl),
		(	lookup_pair_value(flows, Pairs, Flows) ->
			domain_error(open_api_security_scheme(Name, openIdConnect), invalid(flows, Flows))
		;	true
		).

	validate_api_key_location(Name, In) :-
		(	valid_api_key_location(In) ->
			true
		;	domain_error(open_api_security_scheme(Name, apiKey), invalid(in, In))
		).

	valid_api_key_location(query).
	valid_api_key_location(header).
	valid_api_key_location(cookie).

	require_security_scheme_field(Name, Type, Pairs, Field, Value) :-
		(	lookup_pair_value(Field, Pairs, Value) ->
			true
		;	domain_error(open_api_security_scheme(Name, Type), missing(Field))
		).

	validate_oauth_flows_object(Name, Type, Flows) :-
		^^json_object_pairs(Flows, FlowPairs),
		!,
		validate_oauth_flow_pairs(Name, Type, FlowPairs, 0, Count),
		(	Count > 0 ->
			true
		;	domain_error(open_api_security_scheme(Name, Type), missing(oauth_flow))
		).
	validate_oauth_flows_object(Name, Type, Flows) :-
		domain_error(open_api_security_scheme(Name, Type), invalid(flows, Flows)).

	validate_oauth_flow_pairs(_, _, [], Count, Count).
	validate_oauth_flow_pairs(Name, Type, [FlowName-FlowObject| FlowPairs], Count0, Count) :-
		(	oauth_flow_name(FlowName) ->
			validate_oauth_flow_object(Name, Type, FlowName, FlowObject),
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		validate_oauth_flow_pairs(Name, Type, FlowPairs, Count1, Count).

	oauth_flow_name(implicit).
	oauth_flow_name(password).
	oauth_flow_name(clientCredentials).
	oauth_flow_name(authorizationCode).

	validate_oauth_flow_object(Name, Type, FlowName, FlowObject) :-
		^^json_object_pairs(FlowObject, FlowPairs),
		!,
		validate_oauth_flow_required_fields(Name, Type, FlowName, FlowPairs),
		validate_oauth_flow_url_fields(Name, Type, FlowName, FlowPairs).
	validate_oauth_flow_object(Name, Type, FlowName, FlowObject) :-
		domain_error(open_api_security_scheme(Name, Type), invalid(oauth_flow(FlowName), FlowObject)).

	validate_oauth_flow_required_fields(Name, Type, implicit, FlowPairs) :-
		require_oauth_flow_field(Name, Type, implicit, FlowPairs, authorizationUrl, _),
		require_oauth_flow_field(Name, Type, implicit, FlowPairs, scopes, _).
	validate_oauth_flow_required_fields(Name, Type, password, FlowPairs) :-
		require_oauth_flow_field(Name, Type, password, FlowPairs, tokenUrl, _),
		require_oauth_flow_field(Name, Type, password, FlowPairs, scopes, _).
	validate_oauth_flow_required_fields(Name, Type, clientCredentials, FlowPairs) :-
		require_oauth_flow_field(Name, Type, clientCredentials, FlowPairs, tokenUrl, _),
		require_oauth_flow_field(Name, Type, clientCredentials, FlowPairs, scopes, _).
	validate_oauth_flow_required_fields(Name, Type, authorizationCode, FlowPairs) :-
		require_oauth_flow_field(Name, Type, authorizationCode, FlowPairs, authorizationUrl, _),
		require_oauth_flow_field(Name, Type, authorizationCode, FlowPairs, tokenUrl, _),
		require_oauth_flow_field(Name, Type, authorizationCode, FlowPairs, scopes, _).

	require_oauth_flow_field(Name, Type, FlowName, FlowPairs, Field, Value) :-
		(	lookup_pair_value(Field, FlowPairs, Value) ->
			true
		;	domain_error(open_api_security_scheme(Name, Type), missing(oauth_flow_field(FlowName, Field)))
		).

	validate_oauth_flow_url_fields(Name, Type, FlowName, FlowPairs) :-
		validate_oauth_flow_url_field(Name, Type, FlowName, FlowPairs, authorizationUrl),
		validate_oauth_flow_url_field(Name, Type, FlowName, FlowPairs, tokenUrl),
		validate_oauth_flow_url_field(Name, Type, FlowName, FlowPairs, refreshUrl).

	validate_oauth_flow_url_field(Name, Type, FlowName, FlowPairs, Field) :-
		(	lookup_pair_value(Field, FlowPairs, URL) ->
			validate_security_scheme_url_field(Name, Type, oauth_flow_field(FlowName, Field), URL)
		;	true
		).

	validate_security_scheme_url_field(Name, Type, Field, URL) :-
		(	valid_provider_url(URL) ->
			true
		;	domain_error(open_api_security_scheme(Name, Type), invalid_url(Field, URL))
		).

	ignored_parameter(Name, In) :-
		In == header,
		reserved_header_parameter_name(Name).

	reserved_header_parameter_name(Name) :-
		atom(Name),
		ascii_lower_atom(Name, LowerName),
		memberchk(LowerName, [accept, 'content-type', authorization]).

	extension_key(Name) :-
		atom(Name),
		sub_atom(Name, 0, 2, _, 'x-').

	ascii_lower_atom(Atom, LowerAtom) :-
		atom_codes(Atom, Codes),
		ascii_lower_codes(Codes, LowerCodes),
		atom_codes(LowerAtom, LowerCodes).

	ascii_lower_codes([], []).
	ascii_lower_codes([Code| Codes], [LowerCode| LowerCodes]) :-
		ascii_lower_code(Code, LowerCode),
		ascii_lower_codes(Codes, LowerCodes).

	ascii_lower_code(Code, LowerCode) :-
		(	Code >= 0'A,
			Code =< 0'Z ->
			LowerCode is Code + 32
		;	LowerCode = Code
		).

	validate_component_member_name(Context, Name) :-
		(	valid_component_member_name(Name) ->
			true
		;	domain_error(Context, invalid_name(Name))
		).

	valid_component_member_name(Name) :-
		atom(Name),
		atom_codes(Name, Codes),
		Codes \== [],
		valid_component_member_name_codes(Codes).

	valid_component_member_name_codes([]).
	valid_component_member_name_codes([Code| Codes]) :-
		valid_component_member_name_code(Code),
		valid_component_member_name_codes(Codes).

	valid_component_member_name_code(Code) :-
		Code >= 0'0,
		Code =< 0'9,
		!.
	valid_component_member_name_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z,
		!.
	valid_component_member_name_code(Code) :-
		Code >= 0'a,
		Code =< 0'z,
		!.
	valid_component_member_name_code(0'.) :-
		!.
	valid_component_member_name_code(0'_) :-
		!.
	valid_component_member_name_code(0'-).

	validate_provider_url_reference(Context, URL) :-
		(	valid_provider_url_reference(URL) ->
			true
		;	domain_error(Context, invalid_url(URL))
		).

	valid_provider_url(URL) :-
		catch(url(_)::valid(URL), _, fail).

	valid_provider_url_reference(URL) :-
		catch(url(atom)::parse(URL, _, _), _, fail).

	validate_server_url(URL) :-
		(	valid_server_url(URL) ->
			true
		;	domain_error(open_api_server, invalid_url(URL))
		).

	valid_server_url(URL) :-
		atom(URL),
		normalize_server_url_templates(URL, NormalizedURL),
		(	valid_provider_url(NormalizedURL) ->
			true
		;	valid_relative_server_url_reference(NormalizedURL)
		).

	normalize_server_url_templates(URL, NormalizedURL) :-
		atom_codes(URL, Codes),
		normalize_server_url_template_codes(Codes, NormalizedCodes),
		NormalizedCodes \== [],
		atom_codes(NormalizedURL, NormalizedCodes).

	normalize_server_url_template_codes([], []).
	normalize_server_url_template_codes([0'{| Codes], [0'1| NormalizedCodes]) :-
		!,
		Codes = [TemplateCode| _],
		TemplateCode =\= 0'},
		consume_server_url_template_codes(Codes, RestCodes),
		normalize_server_url_template_codes(RestCodes, NormalizedCodes).
	normalize_server_url_template_codes([0'}| _], _) :-
		!,
		fail.
	normalize_server_url_template_codes([Code| Codes], [Code| NormalizedCodes]) :-
		normalize_server_url_template_codes(Codes, NormalizedCodes).

	consume_server_url_template_codes([0'}| Codes], Codes) :-
		!.
	consume_server_url_template_codes([Code| Codes], RestCodes) :-
		Code =\= 0'{,
		consume_server_url_template_codes(Codes, RestCodes).

	valid_relative_server_url_reference(URL) :-
		catch(url(atom)::parse(URL, _, Kind), _, fail),
		Kind \== url.

	schema_term_to_json(schema_ref(Name), Reference) :-
		!,
		schema_ref_object(Name, Reference).
	schema_term_to_json([], []) :-
		!.
	schema_term_to_json([Term| Terms], [JsonTerm| JsonTerms]) :-
		!,
		schema_term_to_json(Term, JsonTerm),
		schema_term_to_json(Terms, JsonTerms).
	schema_term_to_json(Term, Json) :-
		^^json_object_pairs(Term, Pairs),
		!,
		schema_pairs_to_json_pairs(Pairs, JsonPairs),
		^^pairs_to_object(JsonPairs, Json).
	schema_term_to_json(Term, Term).

	schema_pairs_to_json_pairs([], []).
	schema_pairs_to_json_pairs([Pair| Pairs], [Key-JsonValue| JsonPairs]) :-
		^^pair_key_value(Pair, Key, Value),
		schema_term_to_json(Value, JsonValue),
		schema_pairs_to_json_pairs(Pairs, JsonPairs).

	schema_ref_object(Name, {'$ref'-Ref}) :-
		atom_concat('#/components/schemas/', Name, Ref).

	% validation schema

	open_api_document_schema(Schema) :-
		info_object_schema(InfoSchema),
		server_object_schema(ServerSchema),
		paths_object_schema(PathsSchema),
		components_object_schema(ComponentsSchema),
		security_requirement_object_schema(SecurityRequirementSchema),
		external_docs_object_schema(ExternalDocsSchema),
		Schema = {
			type-object,
			required-[openapi, info],
			properties-{
				openapi-{const-'3.1.0'},
				info-InfoSchema,
				servers-{type-array, items-ServerSchema},
				paths-PathsSchema,
				webhooks-PathsSchema,
				components-ComponentsSchema,
				security-{type-array, items-SecurityRequirementSchema},
				externalDocs-ExternalDocsSchema
			},
			anyOf-[{required-[paths]}, {required-[components]}, {required-[webhooks]}],
			additionalProperties- @true
		}.

	info_object_schema(Schema) :-
		contact_object_schema(ContactSchema),
		license_object_schema(LicenseSchema),
		Schema = {
			type-object,
			required-[title, version],
			properties-{
				title-{type-string},
				summary-{type-string},
				description-{type-string},
				termsOfService-{type-string, format-'uri-reference'},
				contact-ContactSchema,
				license-LicenseSchema,
				version-{type-string}
			},
			additionalProperties- @true
		}.

	contact_object_schema({
		type-object,
		properties-{
			name-{type-string},
			url-{type-string, format-'uri-reference'},
			email-{type-string, format-email}
		},
		additionalProperties- @true
	}).

	license_object_schema({
		type-object,
		required-[name],
		properties-{
			name-{type-string},
			identifier-{type-string},
			url-{type-string, format-'uri-reference'}
		},
		additionalProperties- @true
	}).

	server_object_schema({
		type-object,
		required-[url],
		properties-{
			url-{type-string},
			description-{type-string}
		},
		additionalProperties- @true
	}).

	paths_object_schema({
		type-object,
		additionalProperties-{type-object}
	}).

	components_object_schema(Schema) :-
		security_scheme_object_schema(SecuritySchemeSchema),
		Schema = {
			type-object,
			properties-{
				schemas-{type-object, additionalProperties- @true},
				securitySchemes-{type-object, additionalProperties-SecuritySchemeSchema}
			},
			additionalProperties- @true
		}.

	security_requirement_object_schema({
		type-object,
		additionalProperties-{type-array, items-{type-string}}
	}).

	security_scheme_object_schema(Schema) :-
		oauth_flows_object_schema(OAuthFlowsSchema),
		Schema = {
			type-object,
			required-[type],
			properties-{
				type-{type-string, enum-[apiKey, http, mutualTLS, oauth2, openIdConnect]},
				description-{type-string},
				name-{type-string},
				in-{type-string, enum-[query, header, cookie]},
				scheme-{type-string},
				bearerFormat-{type-string},
				flows-OAuthFlowsSchema,
				openIdConnectUrl-{type-string}
			},
			additionalProperties- @true
		}.

	oauth_flows_object_schema(Schema) :-
		oauth_flow_object_schema(OAuthFlowSchema),
		Schema = {
			type-object,
			properties-{
				implicit-OAuthFlowSchema,
				password-OAuthFlowSchema,
				clientCredentials-OAuthFlowSchema,
				authorizationCode-OAuthFlowSchema
			},
			additionalProperties- @true
		}.

	oauth_flow_object_schema({
		type-object,
		properties-{
			authorizationUrl-{type-string},
			tokenUrl-{type-string},
			refreshUrl-{type-string},
			scopes-{type-object, additionalProperties-{type-string}}
		},
		additionalProperties- @true
	}).

	external_docs_object_schema({
		type-object,
		required-[url],
		properties-{
			description-{type-string},
			url-{type-string, format-'uri-reference'}
		},
		additionalProperties- @true
	}).

	% scalar and object helpers

	status_key(default, default) :-
		!.
	status_key(Status, Status) :-
		response_status_range(Status, _, _),
		!.
	status_key(Status, Key) :-
		integer(Status),
		!,
		number_codes(Status, Codes),
		atom_codes(Key, Codes).
	status_key(Status, _) :-
		domain_error(open_api_response_status, Status).

	response_status_range(Status, Minimum, Maximum) :-
		atom(Status),
		atom_codes(Status, [Digit, 0'X, 0'X]),
		Digit >= 0'1,
		Digit =< 0'5,
		Class is Digit - 0'0,
		Minimum is Class*100,
		Maximum is Minimum + 99.

	operation_method(get).
	operation_method(put).
	operation_method(post).
	operation_method(delete).
	operation_method(options).
	operation_method(head).
	operation_method(patch).
	operation_method(trace).

	json_true(@true).
	json_true(true).

	bool_to_json(true, @true).
	bool_to_json(false, @false).

:- end_object.
