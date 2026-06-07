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
		date is 2026-05-23,
		comment is 'Unit tests for the "http_multipart" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(http_multipart).

	test(http_multipart_body_3_01, deterministic(Body == content('multipart/form-data', multipart([Part])))) :-
		http_multipart::part([], content('text/plain', text(hello)), [], Part),
		http_multipart::body('multipart/form-data', [Part], Body).

	test(http_multipart_body_3_02, error(domain_error(http_multipart_media_type, 'text/plain'))) :-
		http_multipart::body('text/plain', [], _Body).

	test(http_multipart_is_body_1_01, true) :-
		Body = content('multipart/form-data', multipart([])),
		http_multipart::is_body(Body).

	test(http_multipart_is_body_1_02, false) :-
		Body = content('text/plain', multipart([])),
		http_multipart::is_body(Body).

	test(http_multipart_part_4_01, deterministic(Part == part([content_disposition-'form-data; name="greeting"'], content('text/plain', text(hello)), []))) :-
		http_multipart::part([content_disposition-'form-data; name="greeting"'], content('text/plain', text(hello)), [], Part).

	test(http_multipart_part_4_02, error(domain_error(http_multipart_part, part(['Content-Type'-'text/plain'], content('text/plain', text(hello)), [])))) :-
		http_multipart::part(['Content-Type'-'text/plain'], content('text/plain', text(hello)), [], _Part).

	test(http_multipart_is_part_1_01, true) :-
		Part = part([content_type-'text/plain'], content('text/plain', text(hello)), []),
		http_multipart::is_part(Part).

	test(http_multipart_is_part_1_02, false) :-
		Part = part(['Content-Type'-'text/plain'], content('text/plain', text(hello)), []),
		http_multipart::is_part(Part).

	test(http_multipart_accessors_2_01, deterministic) :-
		Part = part([content_type-'text/plain'], content('text/plain', text(hello)), [decoded_body(true)]),
		Body = content('multipart/form-data', multipart([Part])),
		http_multipart::media_type(Body, 'multipart/form-data'),
		http_multipart::parts(Body, [Part]),
		http_multipart::part_headers(Part, [content_type-'text/plain']),
		http_multipart::part_body(Part, content('text/plain', text(hello))),
		http_multipart::part_properties(Part, [decoded_body(true)]).

	test(http_multipart_parse_4_01, deterministic) :-
		http_multipart::parse(
			atom('--abc\r\ncontent-type: text/plain\r\n\r\nhello\r\n--abc--\r\n'),
			'multipart/form-data',
			[boundary(abc)],
			Body
		),
		Body = content('multipart/form-data', multipart([Part])),
		Part = part(Headers, content('text/plain', text(hello)), Properties),
		memberchk(content_type-media_type('text/plain', []), Headers),
		memberchk(content_type('text/plain', []), Properties),
		memberchk(decoded_body(true), Properties).

	test(http_multipart_generate_3_01, deterministic(BodyAtom == '--abc\r\ncontent-type: text/plain\r\n\r\nhello\r\n--abc--\r\n')) :-
		Body = content('multipart/form-data', multipart([
			part([], content('text/plain', text(hello)), [])
		])),
		http_multipart::generate(atom(BodyAtom), Body, [boundary(abc)]).

	test(http_multipart_field_part_3_01, deterministic(Part == part([content_disposition-'form-data; name="title"'], content('text/plain', text('Logtalk')), []))) :-
		http_multipart::field_part(title, 'Logtalk', Part).

	test(http_multipart_field_3_01, deterministic((Name == title, Value == 'Logtalk'))) :-
		Part = part([content_disposition-'form-data; name="title"'], content('text/plain', text('Logtalk')), []),
		http_multipart::field(Part, Name, Value).

	test(http_multipart_field_3_02, false) :-
		Part = part([content_disposition-'form-data; name="upload"; filename="notes.txt"'], content('text/plain', text('Logtalk')), []),
		http_multipart::field(Part, _Name, _Value).

	test(http_multipart_fields_2_01, deterministic(Fields == [title-'Logtalk', summary-'Portable'])) :-
		Body = content('multipart/form-data', multipart([
			part([content_disposition-'form-data; name="title"'], content('text/plain', text('Logtalk')), []),
			part([content_disposition-'form-data; name="upload"; filename="notes.txt"'], content('text/plain', text('content')), []),
			part([content_disposition-'form-data; name="summary"'], content('text/plain', text('Portable')), [])
		])),
		http_multipart::fields(Body, Fields).

	test(http_multipart_file_part_5_01, deterministic(Part == part([content_disposition-'form-data; name="upload"; filename="notes.txt"'], content('text/plain', text('hello')), []))) :-
		http_multipart::file_part(upload, 'notes.txt', 'text/plain', text('hello'), Part).

	test(http_multipart_file_5_01, deterministic((Name == upload, Filename == 'notes.txt', MediaType == 'text/plain', Payload == text('hello')))) :-
		Part = part([content_disposition-'form-data; name="upload"; filename="notes.txt"'], content('text/plain', text('hello')), []),
		http_multipart::file(Part, Name, Filename, MediaType, Payload).

	test(http_multipart_file_5_02, false) :-
		Part = part([content_disposition-'form-data; name="title"'], content('text/plain', text('Logtalk')), []),
		http_multipart::file(Part, _Name, _Filename, _MediaType, _Payload).

	test(http_multipart_files_2_01, deterministic(Files == [file(upload, 'notes.txt', 'text/plain', text('hello')), file(archive, 'bundle.bin', 'application/octet-stream', binary([1,2,3]))])) :-
		Body = content('multipart/form-data', multipart([
			part([content_disposition-'form-data; name="title"'], content('text/plain', text('Logtalk')), []),
			part([content_disposition-'form-data; name="upload"; filename="notes.txt"'], content('text/plain', text('hello')), []),
			part([content_disposition-'form-data; name="archive"; filename="bundle.bin"'], content('application/octet-stream', binary([1,2,3])), [])
		])),
		http_multipart::files(Body, Files).

	test(http_multipart_form_data_body_2_01, deterministic) :-
		http_multipart::form_data_body([
			field(title, 'Logtalk'),
			file(upload, 'notes.txt', 'text/plain', text('hello'))
		], Body),
		Body = content('multipart/form-data', multipart([FieldPart, FilePart])),
		FieldPart = part([content_disposition-'form-data; name="title"'], content('text/plain', text('Logtalk')), []),
		FilePart = part([content_disposition-'form-data; name="upload"; filename="notes.txt"'], content('text/plain', text('hello')), []).

	test(http_multipart_parse_4_02, deterministic) :-
		http_multipart::parse(
			atom('--abc\r\ncontent-disposition: form-data; name="title"\r\n\r\nLogtalk\r\n--abc\r\ncontent-disposition: form-data; name="upload"; filename="notes.txt"\r\ncontent-type: text/plain\r\n\r\nhello\r\n--abc--\r\n'),
			'multipart/form-data',
			[boundary(abc)],
			Body
		),
		http_multipart::fields(Body, [title-'Logtalk']),
		http_multipart::files(Body, [file(upload, 'notes.txt', 'text/plain', text(hello))]).

	test(http_multipart_fields_2_02, deterministic(Fields == [])) :-
		Body = content('multipart/form-data', multipart([
			part([content_disposition-'form-data; filename="notes.txt"'], content('text/plain', text('hello')), []),
			part([], content('text/plain', text('Logtalk')), [])
		])),
		http_multipart::fields(Body, Fields).

	test(http_multipart_files_2_02, deterministic(Files == [])) :-
		Body = content('multipart/form-data', multipart([
			part([content_disposition-'form-data; name="upload"'], content('text/plain', text('hello')), []),
			part([content_disposition-'attachment; filename="notes.txt"'], content('text/plain', text('hello')), [])
		])),
		http_multipart::files(Body, Files).

:- end_object.
