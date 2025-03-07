%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-05-09,
		comment is 'Unit tests for the "xml_parser" library.'
	]).

	cover(xml).

	test(xml_parser_access_attribute, true(Year == "1994")) :-
		^^file_path('bib.xml', Path),
		reader::file_to_codes(Path, Codes),
		xml::parse(Codes, Bibliography),
		xml::subterm(Bibliography, element(book, Attributes, _)),
		list::member(year=Year, Attributes),
		Year @> "1993".

	test(xml_parser_access_element, true(Title == "TCP/IP Illustrated")) :-
		^^file_path('bib.xml', Path),
		reader::file_to_codes(Path, Codes),
		xml::parse(Codes, Bibliography),
		xml::subterm(Bibliography, element(book, Attributes, Content)),
		list::member(year=Year, Attributes),
		Year @> "1993",
		xml::subterm(Content, element(title, _, [pcdata(Title)])).

:- end_object.
