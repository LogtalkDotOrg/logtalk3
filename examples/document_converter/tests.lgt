%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-08-16,
		comment is 'Tests for the "document_converter" example.'
	]).

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

:- end_object.
