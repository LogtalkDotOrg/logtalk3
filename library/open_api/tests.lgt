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
		date is 2026-06-08,
		comment is 'Unit tests for the "open_api" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(open_api, [
		document/2, parse/2, generate/2, validate_document/1, validate_document/2, operation/3,
		json_media_descriptor/2, json_media_descriptor/3, json_request_body_descriptor/4,
		json_request_body_descriptor/5, json_response_descriptor/4, json_response_descriptor/5,
		problem_json_schema/1, problem_response_descriptor/3, validate_request/3, validate_request/4,
		validate_http_request/3, validate_response/3, validate_response/4, validate_http_response/3
	]).

	cover(open_api).
	cover(sample_open_api_provider).
	cover(boolean_schema_descriptor_helper_provider).

	test(open_api_document_2_01, deterministic) :-
		document(sample_open_api_provider, Document),
		validate_document(Document),
		json_field(Document, openapi, '3.1.0'),
		json_field(Document, info, Info),
		json_field(Info, title, 'Users API'),
		json_field(Info, description, 'Example OpenAPI provider object for request, response, and document validation.'),
		json_field(Info, license, License),
		json_field(License, name, 'Apache-2.0'),
		json_field(Document, security, [DefaultSecurity]),
		json_field(DefaultSecurity, user_oauth, [read_users]),
		json_field(Document, externalDocs, ExternalDocs),
		json_field(ExternalDocs, url, 'https://example.com/apis/users'),
		json_field(Document, components, Components),
		json_field(Components, securitySchemes, SecuritySchemes),
		json_field(SecuritySchemes, user_bearer, UserBearerScheme),
		json_field(UserBearerScheme, type, http),
		json_field(UserBearerScheme, scheme, bearer),
		json_field(UserBearerScheme, bearerFormat, 'JWT'),
		json_field(SecuritySchemes, user_oauth, UserOAuthScheme),
		json_field(UserOAuthScheme, type, oauth2),
		json_field(UserOAuthScheme, flows, OAuthFlows),
		json_field(OAuthFlows, clientCredentials, ClientCredentialsFlow),
		json_field(ClientCredentialsFlow, scopes, OAuthScopes),
		json_field(OAuthScopes, read_users, 'Read user data'),
		json_field(OAuthScopes, write_users, 'Write user data'),
		json_field(Document, paths, Paths),
		json_field(Paths, '/users', UsersPath),
		json_field(UsersPath, post, _),
		json_field(Paths, '/users/{id}', UserByIdPath),
		json_field(UserByIdPath, get, _),
		json_field(UserByIdPath, put, PutOperation),
		json_field(PutOperation, deprecated, @false),
		json_field(PutOperation, security, [OptionalSecurity, RequiredSecurity]),
		OptionalSecurity == {},
		json_field(RequiredSecurity, user_bearer, []).

	test(open_api_parse_generate_2_01, deterministic) :-
		document(sample_open_api_provider, Document),
		generate(atom(JSON), Document),
		parse(atom(JSON), ParsedDocument),
		validate_document(ParsedDocument),
		json_field(ParsedDocument, openapi, '3.1.0').

	test(open_api_operation_3_01, deterministic) :-
		operation(sample_open_api_provider, update_user, Operation),
		Operation = operation(update_user, put, '/users/{id}', 'Update a user', _, _, _, _).

	test(open_api_operation_3_02, error(existence_error(open_api_operation, delete_user))) :-
		operation(sample_open_api_provider, delete_user, _).

	test(open_api_descriptor_helper_2_01, deterministic(MediaDescriptor == media('application/json', {type-string}))) :-
		json_media_descriptor({type-string}, MediaDescriptor).

	test(open_api_descriptor_helper_2_02, deterministic(MediaDescriptor == media('application/json', schema_ref(widget)))) :-
		json_media_descriptor(schema_ref(widget), MediaDescriptor).

	test(open_api_descriptor_helper_2_04, deterministic(MediaDescriptor == media('application/json', true))) :-
		json_media_descriptor(true, MediaDescriptor).

	test(open_api_descriptor_helper_3_01, deterministic(MediaDescriptor == media('application/vnd.example+json', {type-object}))) :-
		json_media_descriptor('application/vnd.example+json', {type-object}, MediaDescriptor).

	test(open_api_descriptor_helper_3_02a, deterministic(MediaDescriptor == media('text/json', {type-string}))) :-
		json_media_descriptor('text/json', {type-string}, MediaDescriptor).

	test(open_api_descriptor_helper_4_01, deterministic(RequestBody == request_body('Widget payload', true, [media('application/json', {type-object})]))) :-
		json_request_body_descriptor('Widget payload', true, {type-object}, RequestBody).

	test(open_api_descriptor_helper_4_07, deterministic(RequestBody == request_body('No payload', false, [media('application/json', false)]))) :-
		json_request_body_descriptor('No payload', false, false, RequestBody).

	test(open_api_descriptor_helper_5_01, deterministic(RequestBody == request_body('Widget payload', false, [media('application/vnd.example+json', {type-object})]))) :-
		json_request_body_descriptor('application/vnd.example+json', 'Widget payload', false, {type-object}, RequestBody).

	test(open_api_descriptor_helper_4_02, deterministic(ResponseDescriptor == response(201, 'Created', [media('application/json', {type-object})]))) :-
		json_response_descriptor(201, 'Created', {type-object}, ResponseDescriptor).

	test(open_api_descriptor_helper_4_03, deterministic(ResponseDescriptor == response('2XX', 'Successful response', [media('application/json', schema_ref(widget))]))) :-
		json_response_descriptor('2XX', 'Successful response', schema_ref(widget), ResponseDescriptor).

	test(open_api_descriptor_helper_4_08, deterministic(ResponseDescriptor == response(200, 'Any JSON', [media('application/json', true)]))) :-
		json_response_descriptor(200, 'Any JSON', true, ResponseDescriptor).

	test(open_api_descriptor_helper_5_02, deterministic(ResponseDescriptor == response(default, 'Error response', [media('application/vnd.example+json', {type-object})]))) :-
		json_response_descriptor(default, 'Error response', 'application/vnd.example+json', {type-object}, ResponseDescriptor).

	test(open_api_descriptor_helper_1_01, deterministic(ProblemSchema == {
		type-object,
		properties-{
			type-{type-string, format-'uri-reference'},
			title-{type-string},
			detail-{type-string},
			status-{type-integer}
		},
		required-[type, title, detail, status],
		additionalProperties- @true
	})) :-
		problem_json_schema(ProblemSchema).

	test(open_api_descriptor_helper_3_02, deterministic(ResponseDescriptor == response(400, 'Bad Request', [media('application/problem+json', ProblemSchema)]))) :-
		problem_json_schema(ProblemSchema),
		problem_response_descriptor(400, 'Bad Request', ResponseDescriptor).

	test(open_api_descriptor_helper_3_05, error(domain_error(open_api_media_type, 'text/plain'))) :-
		json_media_descriptor('text/plain', {type-object}, _MediaDescriptor).

	test(open_api_descriptor_helper_4_04, error(domain_error(open_api_response_status, 42))) :-
		json_response_descriptor(42, 'Invalid', {type-object}, _ResponseDescriptor).

	test(open_api_descriptor_helper_4_05, error(domain_error(open_api_response_status, '9XX'))) :-
		json_response_descriptor('9XX', 'Invalid', {type-object}, _ResponseDescriptor).

	test(open_api_descriptor_helper_4_06, error(domain_error(open_api_json_schema_term, 42))) :-
		json_request_body_descriptor('Payload', true, 42, _RequestBody).

	test(open_api_descriptor_helper_2_03, error(domain_error(open_api_json_schema_term, schema_ref(42)))) :-
		json_media_descriptor(schema_ref(42), _MediaDescriptor).

	test(open_api_descriptor_helper_3_03, deterministic) :-
		validate_request(
			json_descriptor_helper_provider,
			create_widget,
			request(
				post,
				origin('/widgets'),
				http(1, 1),
				[],
				content('application/json', json({title-'Guide'})),
				[]
			)
		).

	test(open_api_document_2_25, deterministic) :-
		document(boolean_schema_descriptor_helper_provider, Document),
		validate_document(Document),
		json_field(Document, paths, Paths),
		json_field(Paths, '/feature-flags/check', PathItem),
		json_field(PathItem, post, Operation),
		json_field(Operation, requestBody, RequestBody),
		json_field(RequestBody, content, RequestContent),
		json_field(RequestContent, 'application/json', RequestMedia),
		json_field(RequestMedia, schema, @false),
		json_field(Operation, responses, Responses),
		json_field(Responses, '200', ResponseDescriptor),
		json_field(ResponseDescriptor, content, ResponseContent),
		json_field(ResponseContent, 'application/json', ResponseMedia),
		json_field(ResponseMedia, schema, @true).

	test(open_api_descriptor_helper_3_04, deterministic) :-
		validate_response(
			json_descriptor_helper_provider,
			create_widget,
			response(
				http(1, 1),
				status(201, 'Created'),
				[],
				content('application/json', json({id-'100', title-'Guide'})),
				[]
			)
		).

	test(open_api_document_2_02, error(domain_error(open_api_operation_id, duplicate(shared_operation)))) :-
		document(duplicate_operation_id_provider, _).

	test(open_api_document_2_03, error(existence_error(open_api_security_scheme, missing_bearer))) :-
		document(missing_security_scheme_provider, _).

	test(open_api_document_2_04, error(existence_error(open_api_security_scope(user_oauth), admin_users))) :-
		document(invalid_oauth_scope_provider, _).

	test(open_api_document_2_05, error(domain_error(open_api_security_scheme(api_key), invalid_descriptor({type-apiKey})))) :-
		document(invalid_api_key_security_scheme_provider, _).

	test(open_api_document_2_06, error(domain_error(open_api_security_scheme(user_http), invalid_descriptor({type-http})))) :-
		document(invalid_http_security_scheme_provider, _).

	test(open_api_document_2_07, error(domain_error(open_api_security_scheme(user_oauth), invalid_descriptor({type-oauth2})))) :-
		document(invalid_oauth_security_scheme_provider, _).

	test(open_api_document_2_08, error(domain_error(open_api_security_scheme(user_oidc), invalid_descriptor({type-openIdConnect})))) :-
		document(invalid_openid_security_scheme_provider, _).

	test(open_api_document_2_09, error(domain_error(open_api_security_scheme(api_key, apiKey), invalid(in, body)))) :-
		document(invalid_api_key_location_security_scheme_provider, _).

	test(open_api_document_2_10, error(domain_error(open_api_security_scheme(user_oauth, oauth2), invalid_url(oauth_flow_field(clientCredentials, tokenUrl), 'not a url')))) :-
		document(invalid_oauth_url_security_scheme_provider, _).

	test(open_api_document_2_11, error(domain_error(open_api_security_scheme(user_oidc, openIdConnect), invalid_url(openIdConnectUrl, 'not a url')))) :-
		document(invalid_openid_url_security_scheme_provider, _).

	test(open_api_document_2_12, error(domain_error(open_api_server, invalid_url('not a url')))) :-
		document(invalid_server_url_provider, _).

	test(open_api_document_2_12a, error(domain_error(open_api_server, invalid_url('users[1]')))) :-
		document(invalid_relative_server_url_provider, _).

	test(open_api_document_2_13, error(domain_error(open_api_security_scheme(user_oauth, oauth2), invalid(scope, read_users)))) :-
		document(invalid_oauth_scope_descriptor_provider, _).

	test(open_api_document_2_14, error(domain_error(open_api_path_parameter('/users/{id}', get, user_id), not_in_path_template))) :-
		document(invalid_path_parameter_provider, _).

	test(open_api_document_2_15, deterministic) :-
		document(valid_server_url_provider, Document),
		validate_document(Document),
		json_field(Document, servers, [RelativeServer, TemplatedServer]),
		json_field(RelativeServer, url, '/v1'),
		json_field(TemplatedServer, url, 'https://{username}.gigantic-server.com:{port}/{basePath}').

	test(open_api_document_2_16, deterministic) :-
		document(wildcard_response_provider, Document),
		validate_document(Document),
		json_field(Document, paths, Paths),
		json_field(Paths, '/items', ItemsPath),
		json_field(ItemsPath, get, GetOperation),
		json_field(GetOperation, responses, Responses),
		json_field(Responses, '2XX', RangeResponse),
		json_field(RangeResponse, description, 'Successful response').

	test(open_api_document_2_17, deterministic) :-
		document(security_matrix_provider, Document),
		validate_document(Document),
		json_field(Document, components, Components),
		json_field(Components, securitySchemes, SecuritySchemes),
		json_field(SecuritySchemes, api_key_query, ApiKeyQuery),
		json_field(ApiKeyQuery, in, query),
		json_field(SecuritySchemes, api_key_header, ApiKeyHeader),
		json_field(ApiKeyHeader, description, 'Header based API key'),
		json_field(ApiKeyHeader, in, header),
		json_field(SecuritySchemes, api_key_cookie, ApiKeyCookie),
		json_field(ApiKeyCookie, in, cookie),
		json_field(SecuritySchemes, mtls_plain, MTLSScheme),
		json_field(MTLSScheme, type, mutualTLS),
		json_field(SecuritySchemes, mtls_described, DescribedMTLSScheme),
		json_field(DescribedMTLSScheme, description, 'Mutual TLS'),
		json_field(SecuritySchemes, password_oauth, PasswordOAuthScheme),
		json_field(PasswordOAuthScheme, description, 'Extended OAuth scheme'),
		json_field(PasswordOAuthScheme, flows, PasswordOAuthFlows),
		json_field(PasswordOAuthFlows, password, PasswordFlow),
		json_field(PasswordFlow, refreshUrl, 'https://auth.example.com/oauth/refresh'),
		json_field(PasswordOAuthFlows, authorizationCode, AuthorizationCodeFlow),
		json_field(AuthorizationCodeFlow, tokenUrl, 'https://auth.example.com/oauth/token'),
		json_field(SecuritySchemes, user_oidc, OIDCScheme),
		json_field(OIDCScheme, openIdConnectUrl, 'https://auth.example.com/oidc'),
		\+ json_field(OIDCScheme, flows, _).

	test(open_api_document_2_18, deterministic) :-
		document(payload_matrix_provider, Document),
		validate_document(Document),
		json_field(Document, paths, Paths),
		json_field(Paths, '/profiles/{id}/note', ProfileNotePath),
		json_field(ProfileNotePath, patch, PatchOperation),
		json_field(PatchOperation, requestBody, PatchRequestBody),
		json_field(PatchRequestBody, required, @false),
		json_field(Paths, '/login', LoginPath),
		json_field(LoginPath, post, LoginOperation),
		json_field(LoginOperation, requestBody, LoginRequestBody),
		json_field(LoginRequestBody, description, 'Login form').

	test(open_api_document_2_19, error(domain_error(open_api_parameter('/users', get, id, query), duplicate))) :-
		document(duplicate_query_parameter_provider, _).

	test(open_api_document_2_20, deterministic) :-
		document(ignored_header_parameter_provider, Document),
		validate_document(Document),
		json_field(Document, paths, Paths),
		json_field(Paths, '/profiles', ProfilesPath),
		json_field(ProfilesPath, get, GetOperation),
		json_field(GetOperation, parameters, []).

	test(open_api_document_2_21, error(domain_error(open_api_path('/pets/{name}'), duplicate_template('/pets/{petId}')))) :-
		document(equivalent_path_template_provider, _).

	test(open_api_document_2_22, deterministic) :-
		document(openid_flow_security_scheme_provider, Document),
		validate_document(Document),
		json_field(Document, security, [DefaultSecurity]),
		json_field(DefaultSecurity, user_oidc, [profile]),
		json_field(Document, components, Components),
		json_field(Components, securitySchemes, SecuritySchemes),
		json_field(SecuritySchemes, user_oidc, OIDCScheme),
		json_field(OIDCScheme, openIdConnectUrl, 'https://auth.example.com/oidc'),
		\+ json_field(OIDCScheme, flows, _),
		\+ json_field(OIDCScheme, '$localFlows', _).

	test(open_api_document_2_22a, error(existence_error(open_api_security_scope(user_oidc), admin))) :-
		document(invalid_openid_scope_security_scheme_provider, _).

	test(open_api_document_2_23, deterministic) :-
		document(relative_external_docs_provider, Document),
		validate_document(Document),
		json_field(Document, externalDocs, ExternalDocs),
		json_field(ExternalDocs, url, '/docs').

	test(open_api_document_2_24, error(domain_error(open_api_component_schema, invalid_name('bad/key')))) :-
		document(invalid_component_name_provider, _).

	test(open_api_document_2_29, error(test_provider_hook(security))) :-
		document(throwing_security_hook_provider, _).

	test(open_api_document_2_30, error(test_provider_hook(homepage))) :-
		document(throwing_homepage_hook_provider, _).

	test(open_api_document_2_31, error(test_provider_hook(description))) :-
		document(throwing_description_hook_provider, _).

	test(open_api_document_2_32, error(test_provider_hook(license))) :-
		document(throwing_license_hook_provider, _).

	test(open_api_document_2_33, error(domain_error(open_api_summary, 42))) :-
		document(invalid_operation_summary_provider, _).

	test(open_api_document_2_34, error(domain_error(open_api_description, 42))) :-
		document(invalid_response_description_provider, _).

	test(open_api_document_2_35, error(domain_error(open_api_license, 42))) :-
		document(invalid_license_metadata_provider, _).

	test(open_api_document_2_36, error(domain_error(open_api_security_scheme(user_http, http), invalid(scheme, 123)))) :-
		document(invalid_http_security_scheme_scalar_provider, _).

	test(open_api_document_2_37, error(domain_error(open_api_security_scheme(user_key, apiKey), invalid(name, 123)))) :-
		document(invalid_api_key_security_scheme_scalar_provider, _).

	test(open_api_document_2_38, error(domain_error(open_api_security_scheme(user_oidc, openIdConnect), invalid_url(openIdConnectUrl, 123)))) :-
		document(invalid_openid_security_scheme_scalar_provider, _).

	test(open_api_validate_document_1_01, deterministic) :-
		\+ validate_document({
			openapi-'3.1.0',
			info-{
				title-'Broken OAuth Flow API',
				version-'1.0.0'
			},
			components-{
				securitySchemes-{
					user_oauth-{
						type-oauth2,
						flows-{
							authorizationCode-{
								authorizationUrl-'https://auth.example.com/oauth/authorize',
								scopes-{
									read_users-'Read user data'
								}
							}
						}
					}
				}
			}
		}).

	test(open_api_validate_document_1_02, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Broken Path Parameter API',
				version-'1.0.0'
			},
			paths-{
				'/users/{id}'-{
					get-{
						operationId-get_user,
						summary-'Get user',
						parameters-[
							{name-id, in-path, required- @false, schema-{type-string}}
						],
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users/{id}', get, parameters, id], missing_required(true)), Errors).

	test(open_api_validate_document_1_03, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Webhook Only API',
				version-'1.0.0'
			},
			webhooks-{
				user_created-{
					post-{
						responses-{
							'200'-{description-'Accepted'}
						}
					}
				}
			}
		}).

	test(open_api_validate_document_1_04, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Broken Security Reference API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						security-[{missing_operation-[]}],
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			},
			components-{
				securitySchemes-{
					declared-{
						type-apiKey,
						name-key,
						in-header
					}
				}
			},
			security-[{missing_top-[]}]
		}, Errors),
		memberchk(error([security, missing_top], existence_error(open_api_security_scheme, missing_top)), Errors),
		memberchk(error([paths, '/users', get, security, missing_operation], existence_error(open_api_security_scheme, missing_operation)), Errors).

	test(open_api_validate_document_1_05, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Duplicate Query Parameter API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						parameters-[
							{name-id, in-query, schema-{type-string}},
							{name-id, in-query, schema-{type-string}}
						],
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, parameters], duplicate_parameter(id, query)), Errors).

	test(open_api_validate_document_1_06, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Path Key API',
				version-'1.0.0'
			},
			paths-{
				users-{
					get-{
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, users], invalid_path_name), Errors).

	test(open_api_validate_document_1_07, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Equivalent Path Template API',
				version-'1.0.0'
			},
			paths-{
				'/pets/{petId}'-{
					get-{
						parameters-[
							{name-petId, in-path, required- @true, schema-{type-string}}
						],
						responses-{
							'200'-{description-'OK'}
						}
					}
				},
				'/pets/{name}'-{
					get-{
						parameters-[
							{name-name, in-path, required- @true, schema-{type-string}}
						],
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/pets/{name}'], duplicate_templated_path('/pets/{petId}')), Errors).

	test(open_api_validate_document_1_08, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Unexpected Info Field API',
				version-'1.0.0',
				licence-'bad'
			},
			paths-{}
		}, Errors),
		memberchk(error([info, licence], unexpected_property), Errors).

	test(open_api_validate_document_1_09, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Formatted Fields API',
				version-'1.0.0',
				termsOfService-'not a url',
				contact-{
					url-'still not a url',
					email-'not-an-email'
				},
				license-{
					name-'Apache-2.0',
					url-'also not a url'
				}
			},
			paths-{},
			externalDocs-{
				url-'bad url'
			}
		}, Errors),
		memberchk(error([info, termsOfService], invalid_format('uri-reference')), Errors),
		memberchk(error([info, contact, url], invalid_format('uri-reference')), Errors),
		memberchk(error([info, contact, email], invalid_format(email)), Errors),
		memberchk(error([info, license, url], invalid_format('uri-reference')), Errors),
		memberchk(error([externalDocs, url], invalid_format('uri-reference')), Errors).

	test(open_api_validate_document_1_10, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'OpenID Connect Flows API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_oidc-{
						type-openIdConnect,
						openIdConnectUrl-'https://auth.example.com/oidc',
						flows-{
							implicit-{
								authorizationUrl-'https://auth.example.com/oauth/authorize',
								scopes-{
									profile-'Read profile data'
								}
							}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_oidc, flows], unexpected_property), Errors).

	test(open_api_validate_document_1_11, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Component Name API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				schemas-{
					'bad/key'-{type-string}
				}
			}
		}, Errors),
		memberchk(error([components, schemas, 'bad/key'], invalid_component_name('bad/key')), Errors).

	test(open_api_validate_document_1_12, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Operation Summary API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						summary-42,
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, summary], expected_type(string)), Errors).

	test(open_api_validate_document_1_13, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Missing Operation Responses API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						summary-'List users'
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get], missing_required(responses)), Errors).

	test(open_api_validate_document_1_14, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Response Description API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						responses-{
							'200'-{description-42}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, responses, '200', description], expected_type(string)), Errors).

	test(open_api_validate_document_1_15, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Response Status API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						responses-{
							bogus-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, responses, bogus], invalid_response_status(bogus)), Errors).

	test(open_api_validate_document_1_16, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Operation Tags Type API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						tags-123,
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, tags], expected_type(array)), Errors).

	test(open_api_validate_document_1_17, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Operation Tag Entry API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						tags-[users, 123],
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, tags, 2], expected_type(string)), Errors).

	test(open_api_validate_document_1_18, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Unexpected Operation Property API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						foo-bar,
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		memberchk(error([paths, '/users', get, foo], unexpected_property), Errors).

	test(open_api_validate_document_1_19, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Operation Extension Property API',
				version-'1.0.0'
			},
			paths-{
				'/users'-{
					get-{
						'x-example'-bar,
						responses-{
							'200'-{description-'OK'}
						}
					}
				}
			}
		}, Errors),
		Errors == [].

	test(open_api_validate_document_1_20, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid API Key Security Scheme Name API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_key-{
						type-apiKey,
						name-123,
						in-header
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_key, name], expected_type(string)), Errors).

	test(open_api_validate_document_1_21, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid API Key Security Scheme Location Type API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_key-{
						type-apiKey,
						name-key,
						in-123
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_key, in], expected_type(string)), Errors).

	test(open_api_validate_document_1_22, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid API Key Security Scheme Location Value API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_key-{
						type-apiKey,
						name-key,
						in-body
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_key, in], invalid_api_key_location(body)), Errors).

	test(open_api_validate_document_1_23, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid HTTP Security Scheme API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_http-{
						type-http,
						scheme-123
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_http, scheme], expected_type(string)), Errors).

	test(open_api_validate_document_1_24, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid HTTP Bearer Format API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_http-{
						type-http,
						scheme-bearer,
						bearerFormat-123
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_http, bearerFormat], expected_type(string)), Errors).

	test(open_api_validate_document_1_25, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid OpenID Connect URL Type API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_oidc-{
						type-openIdConnect,
						openIdConnectUrl-123
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_oidc, openIdConnectUrl], expected_type(string)), Errors).

	test(open_api_validate_document_1_26, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid OpenID Connect URL Value API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_oidc-{
						type-openIdConnect,
						openIdConnectUrl-'not a url'
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_oidc, openIdConnectUrl], invalid_url('not a url')), Errors).

	test(open_api_validate_document_1_27, deterministic) :-
		validate_document({
			openapi-'3.1.0',
			info-{
				title-'Invalid Security Scheme Description API',
				version-'1.0.0'
			},
			paths-{},
			components-{
				securitySchemes-{
					user_http-{
						type-http,
						scheme-bearer,
						description-123
					}
				}
			}
		}, Errors),
		memberchk(error([components, securitySchemes, user_http, description], expected_type(string)), Errors).

	test(open_api_validate_request_3_01, deterministic) :-
		validate_request(
			sample_open_api_provider,
			get_user,
			request(
				get,
				origin('/users/11111111-1111-1111-1111-111111111111'),
				http(1, 1),
				[],
				empty,
				[]
			)
		).

	test(open_api_validate_request_3_02, deterministic) :-
		validate_request(
			sample_open_api_provider,
			update_user,
			request(
				put,
				origin('/users/11111111-1111-1111-1111-111111111111'),
				http(1, 1),
				[],
				content('application/json', json({name-'Alice Example', active-true})),
				[query_pairs([verbose-true])]
			)
		).

	test(open_api_validate_request_3_03, deterministic) :-
		validate_request(sample_open_api_provider, update_user, http_request_protocol_fixture).

	test(open_api_validate_request_3_04, deterministic) :-
		validate_request(
			sample_open_api_provider,
			update_user,
			request(
				put,
				origin('/users/11111111-1111-1111-1111-111111111111'),
				http(1, 1),
				[],
				content('application/json', json({name-'Alice Example'})),
				[]
			)
		).

	test(open_api_validate_request_3_05, deterministic) :-
		validate_request(
			parameter_matrix_provider,
			get_session,
			request(
				get,
				absolute([scheme(http), authority('api.example.com'), path('/sessions/session-1'), query('verbose=true'), fragment('')]),
				http(1, 1),
				[etag-'session-tag'],
				empty,
				[query_pairs([verbose-true]), cookies([session_id-'cookie-1']), path_params([id-'session-1'])]
			)
		).

	test(open_api_validate_request_3_06, deterministic) :-
		validate_request(
			payload_matrix_provider,
			patch_profile_note,
			request(
				patch,
				origin('/profiles/profile-1/note'),
				http(1, 1),
				[],
				empty,
				[]
			)
		).

	test(open_api_validate_request_3_07, deterministic) :-
		validate_request(
			payload_matrix_provider,
			patch_profile_note,
			request(
				patch,
				origin('/profiles/profile-1/note'),
				http(1, 1),
				[],
				content('text/plain', text('hello')),
				[]
			)
		).

	test(open_api_validate_request_3_08, deterministic) :-
		validate_request(
			payload_matrix_provider,
			submit_login,
			request(
				post,
				origin('/login'),
				http(1, 1),
				[],
				content('application/x-www-form-urlencoded', form([username-'alice', password-'secret'])),
				[]
			)
		).

	test(open_api_validate_request_3_09, deterministic) :-
		validate_request(
			ignored_header_parameter_provider,
			get_profiles,
			request(
				get,
				origin('/profiles'),
				http(1, 1),
				[],
				empty,
				[]
			)
		).

	test(open_api_validate_request_3_10, deterministic) :-
		validate_request(
			media_range_provider,
			accept_any_payload,
			request(
				post,
				origin('/upload'),
				http(1, 1),
				[],
				content('text/plain', text('hello')),
				[]
			)
		).

	test(open_api_validate_request_3_11, deterministic) :-
		validate_request(parameter_matrix_provider, get_session, http_mixed_case_header_request_protocol_fixture).

	test(open_api_validate_http_request_3_01, deterministic) :-
		validate_http_request(
			sample_open_api_provider,
			update_user,
			atom('PUT /users/11111111-1111-1111-1111-111111111111?verbose=true HTTP/1.1\r\ncontent-type: application/json\r\n\r\n{"name":"Alice Example","active":true}')
		).

	test(open_api_validate_request_4_01, deterministic) :-
		validate_request(
			sample_open_api_provider,
			update_user,
			request(
				put,
				origin('/users/11111111-1111-1111-1111-111111111111'),
				http(1, 1),
				[],
				empty,
				[query_pairs([verbose-maybe])]
			),
			Errors
		),
		memberchk(missing_request_body, Errors),
		memberchk(invalid_parameter(query, verbose, _), Errors).

	test(open_api_validate_request_4_02, deterministic) :-
		validate_request(
			parameter_matrix_provider,
			get_session,
			request(
				get,
				absolute([scheme(http), authority('api.example.com'), path('/sessions/session-1'), query('verbose=true'), fragment('')]),
				http(1, 1),
				[etag-'session-tag'],
				empty,
				[query_pairs([verbose-true]), cookies([session_id-'cookie-1']), path_params([id-'different'])]
			),
			Errors
		),
		memberchk(path_parameter_mismatch(id, 'session-1', different), Errors).

	test(open_api_validate_request_4_03, deterministic) :-
		validate_request(
			parameter_matrix_provider,
			get_session,
			request(
				get,
				origin('/sessions/session-1'),
				http(1, 1),
				[etag-'session-tag'],
				content('text/plain', text('unexpected')),
				[cookies([session_id-'cookie-1'])]
			),
			Errors
		),
		memberchk(unexpected_request_body, Errors).

	test(open_api_validate_request_4_04, deterministic) :-
		validate_request(
			parameter_matrix_provider,
			get_session,
			request(
				post,
				origin('/sessions/session-1'),
				http(1, 1),
				[etag-'session-tag'],
				empty,
				[cookies([session_id-'cookie-1'])]
			),
			Errors
		),
		memberchk(request_method_mismatch(get, post), Errors).

	test(open_api_validate_request_4_05, deterministic) :-
		validate_request(
			payload_matrix_provider,
			patch_profile_note,
			http_invalid_request_body_protocol_fixture,
			Errors
		),
		memberchk(invalid_request_body_term(bogus), Errors).

	test(open_api_validate_request_4_06, deterministic) :-
		validate_request(
			payload_matrix_provider,
			submit_login,
			http_unsupported_request_payload_protocol_fixture,
			Errors
		),
		memberchk(unsupported_request_body_payload(xml), Errors).

	test(open_api_validate_response_3_01, deterministic) :-
		user_json(User),
		validate_response(
			sample_open_api_provider,
			update_user,
			response(
				http(1, 1),
				status(200, 'OK'),
				[],
				content('application/json', json(User)),
				[]
			)
		).

	test(open_api_validate_response_3_02, deterministic) :-
		api_error_json(not_found, 'User not found', Error),
		validate_response(
			sample_open_api_provider,
			get_user,
			response(
				http(1, 1),
				status(404, 'Not Found'),
				[],
				content('application/json', json(Error)),
				[]
			)
		).

	test(open_api_validate_response_3_03, deterministic) :-
		validate_response(sample_open_api_provider, get_user, http_response_protocol_fixture).

	test(open_api_validate_response_3_04, deterministic) :-
		validate_response(
			wildcard_response_provider,
			list_items,
			response(
				http(1, 1),
				status(204, 'No Content'),
				[],
				empty,
				[]
			)
		).

	test(open_api_validate_response_3_05, deterministic) :-
		validate_response(payload_matrix_provider, submit_login, http_integer_status_response_protocol_fixture).

	test(open_api_validate_response_3_06, deterministic) :-
		validate_response(
			media_range_provider,
			download_text,
			response(
				http(1, 1),
				status(200, 'OK'),
				[],
				content('text/plain', text('hello')),
				[]
			)
		).

	test(open_api_validate_http_response_3_01, deterministic) :-
		validate_http_response(
			sample_open_api_provider,
			get_user,
			atom('HTTP/1.1 404 Not Found\r\ncontent-type: application/json\r\n\r\n{"code":"not_found","message":"User not found"}')
		).

	test(open_api_validate_response_4_01, deterministic) :-
		invalid_user_json(User),
		validate_response(
			sample_open_api_provider,
			get_user,
			response(
				http(1, 1),
				status(200, 'OK'),
				[],
				content('application/json', json(User)),
				[]
			),
			Errors
		),
		memberchk(invalid_response_body(200, 'application/json', _), Errors).

	test(open_api_validate_response_4_02, deterministic) :-
		validate_response(
			parameter_matrix_provider,
			get_session,
			response(
				http(1, 1),
				status(200, 'OK'),
				[],
				content('text/plain', text('unexpected')),
				[]
			),
			Errors
		),
		memberchk(unexpected_response_body(200), Errors).

	test(open_api_validate_response_4_03, deterministic) :-
		validate_response(
			payload_matrix_provider,
			submit_login,
			response(
				http(1, 1),
				status(200, 'OK'),
				[],
				empty,
				[]
			),
			Errors
		),
		memberchk(missing_response_body(200), Errors).

	test(open_api_validate_response_4_04, deterministic) :-
		validate_response(
			payload_matrix_provider,
			submit_login,
			http_unsupported_response_payload_protocol_fixture,
			Errors
		),
		memberchk(unsupported_response_body_payload(200, raw), Errors).

	test(open_api_validate_response_4_05, deterministic) :-
		validate_response(
			payload_matrix_provider,
			submit_login,
			http_invalid_response_body_protocol_fixture,
			Errors
		),
		memberchk(invalid_response_body_term(200, invalid_body), Errors).

	% auxiliary predicates

	user_json({
		id-'11111111-1111-1111-1111-111111111111',
		name-'Alice Example',
		email-'alice@example.com',
		active- @true,
		updated_at-'2026-05-16T12:00:00Z'
	}).

	invalid_user_json({
		id-'11111111-1111-1111-1111-111111111111',
		name-'Alice Example',
		email-'alice@example.com',
		active- @true
	}).

	api_error_json(Code, Message, {
		code-Code,
		message-Message
	}).

	json_field({Pairs}, Key, Value) :-
		json_pair(Pairs, Key, Value).

	json_pair((Pair, Rest), Key, Value) :-
		!,
		(	pair_key_value(Pair, Key, Value) ->
			true
		;	json_pair(Rest, Key, Value)
		).
	json_pair(Pair, Key, Value) :-
		pair_key_value(Pair, Key, Value).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

:- end_object.
