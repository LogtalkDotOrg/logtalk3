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
		date is 2026-04-09,
		comment is 'Unit tests for the "mime_types" library.'
	]).

	cover(mime_types).

	test(extension_type_02_01, deterministic(Type == 'text/plain')) :-
		mime_types::extension_type('.txt', Type).

	test(extension_type_03_01, deterministic(Type == 'application/x-7z-compressed')) :-
		mime_types::extension_type('.7z', Type, false).

	test(extension_type_03_02, fail) :-
		mime_types::extension_type('.7z', _, true).

	test(guess_file_type_03_01, deterministic(Type-Encoding == 'application/x-tar'-gzip)) :-
		mime_types::guess_file_type('/tmp/archive.tgz', Type, Encoding).

	test(guess_file_type_03_02, deterministic(Type-Encoding == 'application/x-tar'-gzip)) :-
		mime_types::guess_file_type('/tmp/archive.tar.gz', Type, Encoding).

	test(guess_file_type_03_03, deterministic(Type-Encoding == ''-gzip)) :-
		mime_types::guess_file_type('/tmp/archive.gz', Type, Encoding).

	test(guess_type_03_01, deterministic(Type-Encoding == 'image/png'-'')) :-
		mime_types::guess_type('https://example.com/assets/image.PNG?download=1#top', Type, Encoding).

	test(guess_extension_02_01, deterministic(Extension == '.json')) :-
		mime_types::guess_extension('application/json', Extension).

	test(guess_all_extensions_02_01, deterministic(Extensions == ['.html', '.htm'])) :-
		mime_types::guess_all_extensions('text/html', Extensions).

	test(add_type_03_01, deterministic(Type == 'application/x-stage2'), [cleanup(mime_types::reset)]) :-
		mime_types::add_type('application/x-stage2', '.stage2', false),
		mime_types::extension_type('.stage2', Type),
		\+ mime_types::extension_type('.stage2', _, true).

	test(add_type_03_02, true, [cleanup(mime_types::reset)]) :-
		mime_types::add_type('application/x-only-strict', '.strict-only', true),
		mime_types::extension_type('.strict-only', 'application/x-only-strict', true),
		mime_types::extension_type('.strict-only', 'application/x-only-strict', false).

	test(read_mime_types_02_01, deterministic(Pairs == ['.foo'-'application/x-foo', '.bar'-'application/x-foo', '.baz'-'text/x-baz']), [cleanup(::clean_file(Path))]) :-
		^^file_path('mime_types_test.types', Path),
		::create_text_file(Path, '# comment\napplication/x-foo foo bar\ntext/x-baz baz # trailing comment\n'),
		mime_types::read_mime_types(Path, Pairs).

	test(load_02_01, deterministic(Type == 'application/x-test-file'), [cleanup((mime_types::reset, ::clean_file(Path)))]) :-
		^^file_path('mime_types_load.types', Path),
		::create_text_file(Path, 'application/x-test-file testfile\n'),
		mime_types::load(Path),
		mime_types::extension_type('.testfile', Type).

	test(load_02_02, true, [cleanup((mime_types::reset, ::clean_file(Path)))]) :-
		^^file_path('mime_types_strict.types', Path),
		::create_text_file(Path, 'application/x-test-strict strictfile\n'),
		mime_types::load(Path, true),
		mime_types::extension_type('.strictfile', 'application/x-test-strict', true),
		mime_types::extension_type('.strictfile', 'application/x-test-strict', false).

:- end_object.
