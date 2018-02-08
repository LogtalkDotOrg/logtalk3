%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2018/02/08,
		comment is 'Unit tests for the encoding/1 built-in directive.'
	]).

	test(encoding_1_us_ascii) :-
        file_path('us_ascii.lgt', Path),
        logtalk_load(Path),
		logtalk::loaded_file_property(Path, text_properties(Properties)),
		member(encoding('US-ASCII'), Properties),
		\+ member(encoding('UTF-8'), Properties).

	test(encoding_1_iso_8859_1) :-
        file_path('iso_8859_1.lgt', Path),
        logtalk_load(Path),
		logtalk::loaded_file_property(Path, text_properties(Properties)),
		member(encoding('ISO-8859-1'), Properties),
		\+ member(encoding('UTF-8'), Properties).

	test(encoding_1_utf_8_bom) :-
        file_path('utf_8_bom.lgt', Path),
        logtalk_load(Path),
		logtalk::loaded_file_property(Path, text_properties(Properties)),
		member(encoding('UTF-8'), Properties),
		member(bom(true), Properties).

	test(encoding_1_utf_8_no_bom) :-
        file_path('utf_8_no_bom.lgt', Path),
        logtalk_load(Path),
		logtalk::loaded_file_property(Path, text_properties(Properties)),
		member(encoding('UTF-8'), Properties),
		\+ member(bom(true), Properties).

    % auxiliary predicates

    file_path(File, Path) :-
        this(This),
        object_property(This, file(_, Directory)),
        atom_concat(Directory, File, Path).

	member(Element, [Head| _]) :-
		Element == Head,
		!.
	member(Element, [_| Tail]) :-
		member(Element, Tail).

:- end_object.
