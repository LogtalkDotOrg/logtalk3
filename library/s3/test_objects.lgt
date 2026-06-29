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


:- object(s3_test_data).

	:- public(list_buckets_xml/1).
	:- mode(list_buckets_xml(-atom), one).

	:- public(list_objects_v2_xml/1).
	:- mode(list_objects_v2_xml(-atom), one).

	list_buckets_xml('<?xml version="1.0" encoding="UTF-8"?><ListAllMyBucketsResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><Buckets><Bucket><Name>photos</Name><CreationDate>2024-01-01T00:00:00.000Z</CreationDate></Bucket></Buckets></ListAllMyBucketsResult>').

	list_objects_v2_xml('<?xml version="1.0" encoding="UTF-8"?><ListBucketResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><Name>photos</Name><Prefix>2024/</Prefix><KeyCount>1</KeyCount><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated><Contents><Key>2024/report.txt</Key><LastModified>2024-06-01T12:00:00.000Z</LastModified><ETag>&quot;etag-1&quot;</ETag><Size>5</Size><StorageClass>STANDARD</StorageClass></Contents><CommonPrefixes><Prefix>2024/archive/</Prefix></CommonPrefixes></ListBucketResult>').

:- end_object.


:- object(s3_test_handler,
	implements(http_handler_protocol)).

	:- uses(http_core, [
		body/2, method/2, response/6, target/2
	]).

	handle(Request, Response) :-
		http_core::method(Request, Method),
		http_core::target(Request, Target),
		handle_request(Method, Target, Request, Response).

	handle_request(get, origin('/'), _Request, Response) :-
		s3_test_data::list_buckets_xml(XML),
		atom_codes(XML, Codes),
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[x_amz_request_id-'req-list-buckets'],
			content('application/octet-stream', binary(Codes)),
			[],
			Response
		).
	handle_request(get, origin('/photos', Query), _Request, Response) :-
		sub_atom(Query, _, _, _, 'list-type=2'),
		!,
		s3_test_data::list_objects_v2_xml(XML),
		atom_codes(XML, Codes),
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[x_amz_request_id-'req-list-objects'],
			content('application/octet-stream', binary(Codes)),
			[],
			Response
		).
	handle_request(head, origin('/photos'), _Request, Response) :-
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[x_amz_bucket_region-'us-east-1', x_amz_request_id-'req-head-bucket'],
			empty,
			[],
			Response
		).
	handle_request(head, origin('/photos/report.txt'), _Request, Response) :-
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[
				etag-'"etag-123"',
				x_amz_request_id-'req-head-object',
				x_amz_version_id-'3'
			],
			empty,
			[],
			Response
		).
	handle_request(head, origin('/photos/report.txt', _Query), Request, Response) :-
		handle_request(head, origin('/photos/report.txt'), Request, Response).
	handle_request(head, _Target, _Request, Response) :-
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[
				etag-'"etag-123"',
				x_amz_request_id-'req-head-object',
				x_amz_version_id-'3'
			],
			empty,
			[],
			Response
		).
	handle_request(get, origin('/photos/report.txt'), _Request, Response) :-
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[etag-'"etag-123"', x_amz_request_id-'req-get-object'],
			content('application/octet-stream', binary([104, 101, 108, 108, 111])),
			[],
			Response
		).
	handle_request(put, origin('/photos/report.txt'), Request, Response) :-
		http_core::body(Request, content('application/octet-stream', binary([104, 101, 108, 108, 111]))),
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[etag-'"etag-123"', x_amz_request_id-'req-put-object'],
			empty,
			[],
			Response
		).
	handle_request(delete, origin('/photos/report.txt'), _Request, Response) :-
		http_core::response(
			http(1, 1),
			status(204, 'No Content'),
			[x_amz_request_id-'req-delete-object'],
			empty,
			[],
			Response
		).
	handle_request(put, origin('/archive/report.txt'), _Request, Response) :-
		CopyXML = '<?xml version="1.0" encoding="UTF-8"?><CopyObjectResult><LastModified>2024-06-01T12:00:00.000Z</LastModified><ETag>&quot;etag-copy&quot;</ETag></CopyObjectResult>',
		atom_codes(CopyXML, CopyCodes),
		http_core::response(
			http(1, 1),
			status(200, 'OK'),
			[x_amz_request_id-'req-copy-object'],
			content('application/octet-stream', binary(CopyCodes)),
			[],
			Response
		).
	handle_request(_Method, _Target, _Request, Response) :-
		http_core::response(
			http(1, 1),
			status(404, 'Not Found'),
			[],
			content('text/plain', text('missing')),
			[],
			Response
		).

:- end_object.
