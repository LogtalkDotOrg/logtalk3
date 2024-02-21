%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2024-02-21,
		comment is 'Tests for the "document_converter" example.'
	]).

	:- threaded.

	condition :-
		os::environment_variable('CLASSPATH', CLASSPATH),
		sub_atom(CLASSPATH, _, _, _, 'tika-app-').

	cleanup :-
		^^clean_file('test_files/sample.pdf.txt'),
		^^clean_file('test_files/sample.doc.txt'),
		^^clean_file('test_files/sample.docx.txt'),
		^^clean_file('test_files/sample.odt.txt').

	test(document_converter_convert_pdf_to_text, true(os::file_exists(Target))) :-
		^^file_path('test_files/sample.pdf', Source),
		^^file_path('test_files/sample.pdf.txt', Target),
		document::convert(Source, Target).

	test(document_converter_convert_doc_to_text, true(os::file_exists(Target))) :-
		^^file_path('test_files/sample.doc', Source),
		^^file_path('test_files/sample.doc.txt', Target),
		document::convert(Source, Target).

	test(document_converter_convert_docx_to_text, true(os::file_exists(Target))) :-
		^^file_path('test_files/sample.docx', Source),
		^^file_path('test_files/sample.docx.txt', Target),
		document::convert(Source, Target).

	test(document_converter_convert_odt_to_text, true(os::file_exists(Target))) :-
		^^file_path('test_files/sample.odt', Source),
		^^file_path('test_files/sample.odt.txt', Target),
		document::convert(Source, Target).

	test(document_converter_get_pdf_contents, true(sub_atom(Contents, _, _, _, 'Universal Declaration of Human Rights'))) :-
		^^file_path('test_files/sample.pdf', Source),
		document::contents(Source, Contents).

	test(document_converter_get_doc_contents, true(sub_atom(Contents, _, _, _, 'Universal Declaration of Human Rights'))) :-
		^^file_path('test_files/sample.doc', Source),
		document::contents(Source, Contents).

	test(document_converter_get_docx_contents, true(sub_atom(Contents, _, _, _, 'Universal Declaration of Human Rights'))) :-
		^^file_path('test_files/sample.docx', Source),
		document::contents(Source, Contents).

	test(document_converter_get_odt_contents, true(sub_atom(Contents, _, _, _, 'Universal Declaration of Human Rights'))) :-
		^^file_path('test_files/sample.odt', Source),
		document::contents(Source, Contents).

	test(document_converter_sequential_convert, true, [condition(current_logtalk_flag(threads, supported)), setup(cleanup)]) :-
		^^file_path('test_files/sample.pdf', Source1),
		^^file_path('test_files/sample.pdf.txt', Target1),
		^^file_path('test_files/sample.doc', Source2),
		^^file_path('test_files/sample.doc.txt', Target2),
		^^file_path('test_files/sample.docx', Source3),
		^^file_path('test_files/sample.docx.txt', Target3),
		^^file_path('test_files/sample.odt', Source4),
		^^file_path('test_files/sample.odt.txt', Target4),
		document::convert(Source1, Target1),
		document::convert(Source2, Target2),
		document::convert(Source3, Target3),
		document::convert(Source4, Target4),
		^^assertion(os::file_exists(Target1)),
		^^assertion(os::file_exists(Target2)),
		^^assertion(os::file_exists(Target3)),
		^^assertion(os::file_exists(Target4)).

	test(document_converter_concurrent_convert, true, [condition(current_logtalk_flag(threads, supported)), setup(cleanup)]) :-
		^^file_path('test_files/sample.pdf', Source1),
		^^file_path('test_files/sample.pdf.txt', Target1),
		^^file_path('test_files/sample.doc', Source2),
		^^file_path('test_files/sample.doc.txt', Target2),
		^^file_path('test_files/sample.docx', Source3),
		^^file_path('test_files/sample.docx.txt', Target3),
		^^file_path('test_files/sample.odt', Source4),
		^^file_path('test_files/sample.odt.txt', Target4),
		threaded((
			document::convert(Source1, Target1),
			document::convert(Source2, Target2),
			document::convert(Source3, Target3),
			document::convert(Source4, Target4)
		)),
		^^assertion(os::file_exists(Target1)),
		^^assertion(os::file_exists(Target2)),
		^^assertion(os::file_exists(Target3)),
		^^assertion(os::file_exists(Target4)).

:- end_object.
