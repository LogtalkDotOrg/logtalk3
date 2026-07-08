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
		date is 2026-07-08,
		comment is 'Unit tests for the "s3" library.'
	]).

	:- uses(http_core, [
		body/2, generate_body/3, parse_body/4, response/6
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(os, [
		delete_file/1, temporary_directory/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(s3_client(_)).
	cover(s3_client).
	cover(s3_endpoints).
	cover(s3_headers).
	cover(s3_session(_)).
	cover(s3_signing).
	cover(s3_xml).

	setup :-
		cleanup.

	cleanup :-
		^^clean_file('s3_get_object_report.bin'),
		^^clean_file('s3_put_object_report.unknown'),
		^^clean_file('s3_session_put_report.unknown'),
		^^clean_file('s3_session_get_report.bin').

	test(s3_endpoints_01, true((URL == 'https://photos.s3.eu-west-1.amazonaws.com/2024/report.txt', Endpoint == endpoint('photos.s3.eu-west-1.amazonaws.com', '/2024/report.txt')))) :-
		s3_endpoints::request(head_object, photos, '2024/report.txt', URL, Endpoint, [region('eu-west-1')]).

	test(s3_endpoints_02, true(URI == '/a%20b/~')) :-
		s3_endpoints::canonical_uri(get_object, photos, 'a b/~', URI).

	test(s3_endpoints_03, true((URL == 'http://localhost:9000/base/photos/report.txt', Endpoint == endpoint('localhost:9000', '/base/photos/report.txt')))) :-
		s3_endpoints::request(get_object, photos, 'report.txt', URL, Endpoint, [
			endpoint('http://localhost:9000/base/'),
			addressing_style(path)
		]).

	test(s3_signing_01, true(Query == 'list-type=2&max-keys=100&prefix=photos%2F2024')) :-
		s3_signing::canonical_query(['list-type'-2, prefix-'photos/2024', 'max-keys'-100], Query).

	test(s3_signing_02, true((sub_atom(Request, _, _, _, 'flag=true'), sub_atom(Request, _, _, _, 'x-list:a,b'), sub_atom(Request, _, _, _, 'x-number:42'), sub_atom(Request, _, _, _, 'x-term:foo(bar)')))) :-
		s3_signing::canonical_request(get, '/', [flag-true], [x_number-42, x_list-[a,b], x_term-foo(bar)], 'UNSIGNED-PAYLOAD', Request).

	test(s3_signing_03, true(sub_atom(URL, _, _, _, 'X-Amz-Date=20240607T080905Z'))) :-
		s3_client::presigned_get_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			request_time(date_time(2024, 6, 7, 8, 9, 5))
		]).

	test(s3_headers_01, true(Value == '"etag-123"')) :-
		http_core::response(http(1, 1), status(200, 'OK'), [etag-'"etag-123"'], empty, [], Response),
		s3_headers::header_value(Response, etag, Value).

	test(s3_xml_01, true((member(name(photos), Properties), Entries == [object('2024/report.txt', 5, '"etag-1"', '2024-06-01T12:00:00.000Z', [storage_class('STANDARD')])], Prefixes == [prefix('2024/archive/')])) ) :-
		s3_test_data::list_objects_v2_xml(XML),
		atom_codes(XML, Codes),
		s3_xml::list_objects_v2_response(content('application/octet-stream', binary(Codes)), objects(Properties, Entries, Prefixes)).

	test(s3_xml_02, true(Buckets == buckets([]))) :-
		XML = '<ListAllMyBucketsResult/>',
		s3_xml::list_buckets_response(content('application/xml', text(XML)), Buckets).

	test(s3_xml_03, true(Result == copy_result('"etag-copy"', [last_modified('2024-06-01T12:00:00.000Z')]))) :-
		XML = '<CopyObjectResult><LastModified>2024-06-01T12:00:00.000Z</LastModified><ETag>&quot;etag-copy&quot;</ETag></CopyObjectResult>',
		s3_xml::copy_object_response(content('application/xml', text(XML)), Result).

	test(s3_xml_04, true(Result == multipart_upload(photos, 'report.txt', 'upload-1', []))) :-
		XML = '<InitiateMultipartUploadResult><Bucket>photos</Bucket><Key>report.txt</Key><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>',
		s3_xml::create_multipart_upload_response(content('application/xml', text(XML)), Result).

	test(s3_xml_05, true(Error == s3_error('NoSuchKey', 'Missing', [resource('/photos/missing.txt'), request_id('req-error'), host_id('host-id')]))) :-
		XML = '<Error><Code>NoSuchKey</Code><Message>Missing</Message><Resource>/photos/missing.txt</Resource><RequestId>req-error</RequestId><HostId>host-id</HostId></Error>',
		s3_xml::error_response(content('application/xml', text(XML)), Error).

	test(s3_presign_01, true(URL == 'https://photos.s3.eu-west-1.amazonaws.com/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Feu-west-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=900&X-Amz-Signature=2ae56ff3d2ac54cca5c62f7e390845e7703952e6182dc1a07763b5d3a2ca851f&X-Amz-SignedHeaders=host')) :-
		s3_client::presigned_get_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			amz_date('20240627T120000Z')
		]).

	test(s3_presign_02, true(URL == 'http://127.0.0.1:9000/photos/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=60&X-Amz-Security-Token=token-123&X-Amz-Signature=5eb7cd5ce855e8681e44ac5f8c810029235bdf548d771947cf714c5a297285dd&X-Amz-SignedHeaders=content-type%3Bhost')) :-
		s3_client::presigned_put_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			endpoint('http://127.0.0.1:9000'),
			addressing_style(path),
			amz_date('20240627T120000Z'),
			expires(60),
			session_token('token-123'),
			headers([content_type-'text/plain'])
		]).

	test(s3_presign_03, error(domain_error(s3_presign_expires, 604801))) :-
		s3_client::presigned_get_object(photos, 'report.txt', _URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			expires(604801)
		]).

	test(s3_presign_04, true(URL == 'https://photos.s3.eu-west-1.amazonaws.com/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Feu-west-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=900&X-Amz-Signature=3dadf1e70968cb2ad92f267266a6555dcd255dd251c5d41be0922bbf4d1fb11a&X-Amz-SignedHeaders=host')) :-
		s3_client::presigned_post_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			amz_date('20240627T120000Z')
		]).

	test(s3_presign_05, true(URL == 'http://127.0.0.1:9000/photos/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=60&X-Amz-Signature=c0c05eb21ee9e271deaecf5066814c23248f6d08777df261d09fcbac1786eda3&X-Amz-SignedHeaders=host')) :-
		s3_client::presigned_post_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			endpoint('http://127.0.0.1:9000'),
			addressing_style(path),
			amz_date('20240627T120000Z'),
			expires(60)
		]).

	test(s3_presign_06, true(URL == 'https://photos.s3.eu-west-1.amazonaws.com/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Feu-west-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=900&X-Amz-Signature=d41048d2b4a2e61af7715424247367783be721685568c4b02e1fd94b5d550c6c&X-Amz-SignedHeaders=host')) :-
		PayloadHash = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824',
		s3_client::presigned_put_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			amz_date('20240627T120000Z'),
			payload_hash_mode(signed),
			payload_hash(PayloadHash)
		]).

	test(s3_presign_07, true(URL == 'https://photos.s3.eu-west-1.amazonaws.com/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Feu-west-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=900&X-Amz-Signature=e366f7511c4e9dfef455b16dfead13b40b90c6a1d1cc5a6e9c7c41afbcd16dee&X-Amz-SignedHeaders=host')) :-
		PayloadHash = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824',
		s3_client::presigned_post_object(photos, 'report.txt', URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			amz_date('20240627T120000Z'),
			payload_hash_mode(signed),
			payload_hash(PayloadHash)
		]).

	test(s3_presign_08, error(domain_error(s3_presign_payload_hash, missing))) :-
		s3_client::presigned_put_object(photos, 'report.txt', _URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			payload_hash_mode(signed)
		]).

	test(s3_presign_09, error(domain_error(s3_payload_hash, not_a_hash))) :-
		s3_client::presigned_get_object(photos, 'report.txt', _URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			payload_hash_mode(signed),
			payload_hash(not_a_hash)
		]).

	test(s3_presign_10, error(domain_error(s3_presign_payload_hash, '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824'))) :-
		s3_client::presigned_get_object(photos, 'report.txt', _URL, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			payload_hash('2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824')
		]).

	test(s3_session_presign_01, true(URL == 'https://photos.s3.eu-west-1.amazonaws.com/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Feu-west-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=60&X-Amz-Signature=734906595c0299469192dfa2b2360dc54232827c9762677accd1b1699c85cdc9&X-Amz-SignedHeaders=host')) :-
		s3_session(http_process_transport)::open(Session, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			amz_date('20240627T120000Z')
		]),
		s3_session(http_process_transport)::presigned_get_object(Session, photos, 'report.txt', URL, [expires(60)]),
		s3_session(http_process_transport)::close(Session).

	test(s3_session_presign_02, true(URL == 'https://photos.s3.eu-west-1.amazonaws.com/report.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIDEXAMPLE%2F20240627%2Feu-west-1%2Fs3%2Faws4_request&X-Amz-Date=20240627T120000Z&X-Amz-Expires=60&X-Amz-Signature=3c9723bbb645241d3fb216140a259438054d9e5999ab54a12a855603bbc2c728&X-Amz-SignedHeaders=host')) :-
		s3_session(http_process_transport)::open(Session, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
			region('eu-west-1'),
			amz_date('20240627T120000Z')
		]),
		s3_session(http_process_transport)::presigned_post_object(Session, photos, 'report.txt', URL, [expires(60)]),
		s3_session(http_process_transport)::close(Session).

	test(s3_arg_check_01, error(instantiation_error)) :-
		s3_client::head_bucket(_, _, []).

	test(s3_arg_check_02, error(type_error(atom, 123))) :-
		s3_client::head_bucket(123, _, []).

	test(s3_arg_check_03, error(domain_error(s3_bucket, ''))) :-
		s3_client::head_bucket('', _, []).

	test(s3_arg_check_04, error(instantiation_error)) :-
		s3_client::head_object(photos, _, _, []).

	test(s3_arg_check_05, error(type_error(atom, 123))) :-
		s3_client::head_object(photos, 123, _, []).

	test(s3_arg_check_06, error(instantiation_error)) :-
		s3_client::list_objects_v2(photos, _, _, []).

	test(s3_arg_check_07, error(instantiation_error)) :-
		s3_client::list_objects_v2(photos, list_objects_v2_request([_]), _, []).

	test(s3_arg_check_08, error(type_error(compound, bad))) :-
		s3_client::list_objects_v2(photos, list_objects_v2_request([bad]), _, []).

	test(s3_arg_check_09, error(domain_error(s3_list_objects_request_option, bad(value)))) :-
		s3_client::list_objects_v2(photos, list_objects_v2_request([bad(value)]), _, []).

	test(s3_arg_check_10, error(domain_error(s3_list_objects_request, malformed_request))) :-
		s3_client::list_objects_v2(photos, malformed_request, _, []).

	test(s3_arg_check_11, error(instantiation_error)) :-
		s3_client::copy_object(_, archive, 'report.txt', _, []).

	test(s3_arg_check_12, error(type_error(compound, 123))) :-
		s3_client::copy_object(123, archive, 'report.txt', _, []).

	test(s3_arg_check_13, error(domain_error(s3_copy_source, source(photos)))) :-
		s3_client::copy_object(source(photos), archive, 'report.txt', _, []).

	test(s3_arg_check_14, error(domain_error(s3_version_id, ''))) :-
		s3_client::copy_object(source(photos, 'report.txt', ''), archive, 'report.txt', _, []).

	test(s3_arg_check_15, error(instantiation_error)) :-
		s3_client::put_object(photos, 'report.txt', _, _, _, []).

	test(s3_arg_check_16, error(type_error(atom, 123))) :-
		s3_client::put_object(photos, 'report.txt', 123, _, _, []).

	test(s3_arg_check_17, error(existence_error(file, File))) :-
		^^file_path('non_existing_report.txt', File),
		s3_client::put_object(photos, 'non_existing_report.txt', File, _, _, []).

	test(s3_arg_check_18, error(instantiation_error)) :-
		s3_client::get_object(photos, 'report.txt', _, _, []).

	test(s3_arg_check_19, error(type_error(atom, 123))) :-
		s3_client::get_object(photos, 'report.txt', 123, _, []).

	test(s3_arg_check_20, error(existence_error(directory, Directory))) :-
		os::temporary_directory(TemporaryDirectory),
		atomic_list_concat([TemporaryDirectory, '/s3_missing_parent_argument_checks/'], Directory),
		atomic_list_concat([Directory, 'report.bin'], File),
		s3_client::get_object(photos, 'report.txt', File, _, []).

	test(s3_arg_check_22, error(domain_error(s3_bucket, '')), [cleanup((ground(Session) -> s3_session(http_process_transport)::close(Session); true))]) :-
		s3_session(http_process_transport)::open(Session, []),
		s3_session(http_process_transport)::head_bucket(Session, '', _, []).

	test(s3_arg_check_23, error(instantiation_error), [cleanup((ground(Session) -> s3_session(http_process_transport)::close(Session); true))]) :-
		s3_session(http_process_transport)::open(Session, [
			credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret'))
		]),
		s3_session(http_process_transport)::presigned_get_object(Session, photos, 'report.txt', _, _).

	test(s3_arg_check_24, error(instantiation_error)) :-
		s3_session(http_process_transport)::list_buckets(_, _, []).

	test(s3_arg_check_25, error(domain_error(s3_session, not_a_session))) :-
		s3_session(http_process_transport)::list_buckets(not_a_session, _, []).

	test(s3_arg_check_26, error(existence_error(s3_session, s3_session(999999)))) :-
		s3_session(http_process_transport)::list_buckets(s3_session(999999), _, []).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(s3_client_01, true(Buckets == buckets([bucket(photos, '2024-01-01T00:00:00.000Z', [])]))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::list_buckets(Buckets, Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_03, true((Bytes == [104, 101, 108, 108, 111], member(header(etag, '"etag-123"'), Properties)))) :-
			^^file_path('s3_get_object_report.bin', File),
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::get_object(photos, 'report.txt', File, Properties, Options),
			http_core::parse_body(file(File), 'application/octet-stream', [], content('application/octet-stream', binary(Bytes))),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_05, true((ETag == '"etag-123"', member(header(etag, '"etag-123"'), Properties)))) :-
			^^file_path('s3_put_object_report.unknown', File),
			http_core::generate_body(file(File), content('application/octet-stream', binary([104, 101, 108, 108, 111])), []),
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::put_object(photos, 'report.txt', File, ETag, Properties, Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_06, true(member(header(x_amz_bucket_region, 'us-east-1'), Properties))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::head_bucket(photos, bucket_metadata(Properties), Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_07, true((member(name(photos), Properties), member(prefix('2024/archive/'), Prefixes)))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			Request = list_objects_v2_request([
				delimiter('/'),
				continuation_token('token'),
				encoding_type(url),
				fetch_owner(true),
				max_keys(100),
				prefix('2024/'),
				start_after('2023/')
			]),
			s3_client::list_objects_v2(photos, Request, objects(Properties, _Entries, Prefixes), Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_08, true((member(header(etag, '"etag-123"'), Properties), member(header(x_amz_version_id, '3'), Properties)))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::head_object(photos, 'report.txt', object_metadata(Properties), Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_09, true(member(status(204, 'No Content'), Properties))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::delete_object(photos, 'report.txt', delete_result(Properties), Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_10, true(Result == copy_result('"etag-copy"', [last_modified('2024-06-01T12:00:00.000Z')]))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::copy_object(source(photos, 'report.txt'), archive, 'report.txt', Result, Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_client_11, true(Result == copy_result('"etag-copy"', [last_modified('2024-06-01T12:00:00.000Z')]))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_client::copy_object(source(photos, 'report.txt', '3'), archive, 'report.txt', Result, Options),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_03, true(Buckets == buckets([bucket(photos, '2024-01-01T00:00:00.000Z', [])]))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session),
			s3_session(http_process_transport)::list_buckets(Session, Buckets, Options),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_04, true(member(header(x_amz_bucket_region, 'us-east-1'), Properties))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::head_bucket(Session, photos, bucket_metadata(Properties), []),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_05, true(member(name(photos), Properties))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::list_objects_v2(Session, photos, [prefix('2024/')], objects(Properties, _Entries, _Prefixes), []),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_06, true(member(header(etag, '"etag-123"'), Properties))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::head_object(Session, photos, 'report.txt', object_metadata(Properties), []),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_07, true((Bytes == [104, 101, 108, 108, 111], member(header(etag, '"etag-123"'), Properties)))) :-
			^^file_path('s3_session_get_report.bin', File),
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::get_object(Session, photos, 'report.txt', File, Properties, []),
			s3_session(http_process_transport)::close(Session),
			http_core::parse_body(file(File), 'application/octet-stream', [], content('application/octet-stream', binary(Bytes))),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_08, true(sub_atom(URL, _, _, _, 'X-Amz-Signature='))) :-
			request_options(9000, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::presigned_put_object(Session, photos, 'report.txt', URL, [expires(60)]),
			s3_session(http_process_transport)::close(Session).

		test(s3_session_10, true(member(status(204, 'No Content'), Properties))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::delete_object(Session, photos, 'report.txt', delete_result(Properties), []),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_11, true(Result == copy_result('"etag-copy"', [last_modified('2024-06-01T12:00:00.000Z')]))) :-
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::copy_object(Session, source(photos, 'report.txt'), archive, 'report.txt', Result, []),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		test(s3_session_02, true((ETag == '"etag-123"', member(header(etag, '"etag-123"'), Properties)))) :-
			^^file_path('s3_session_put_report.unknown', File),
			http_core::generate_body(file(File), content('application/octet-stream', binary([104, 101, 108, 108, 111])), []),
			open_listener(Port, Listener),
			threaded_once(serve_once(Listener), Tag),
			request_options(Port, Options),
			s3_session(http_process_transport)::open(Session, Options),
			s3_session(http_process_transport)::put_object(Session, photos, 'report.txt', File, ETag, Properties, []),
			s3_session(http_process_transport)::close(Session),
			threaded_exit(serve_once(Listener), Tag),
			http_process_transport::close_listener(Listener).

		open_listener(Port, Listener) :-
			http_process_transport::open_listener('127.0.0.1', Port, Listener, []).

		serve_once(Listener) :-
			catch(http_process_transport::serve_once(Listener, s3_test_handler, _ClientInfo), _, true).

		request_options(Port, Options) :-
			atomic_list_concat(['http://127.0.0.1:', Port], Endpoint),
			Options = [
				endpoint(Endpoint),
				addressing_style(path),
				credentials(access_key_id('AKIDEXAMPLE'), secret_access_key('secret')),
				amz_date('20240627T120000Z')
			].

	:- endif.

:- end_object.
