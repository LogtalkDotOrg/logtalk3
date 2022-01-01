%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		date is 2021-03-13,
		comment is 'Unit tests for the "base64" library.'
	]).

	cover(base64).
	cover(base64url).

	cleanup :-
		this(This),
		object_property(This, file(_, Directory)),
		os::path_concat(Directory, 'test_files/dump_base64.txt', Path),
		catch(os::delete_file(Path), _, true).

	test(base64_parse_2_atom, true) :-
		base64::parse(atom('VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4='), Bytes),
		^^assertion(atom_codes('The quick brown fox jumps over the lazy dog.', Bytes)).

	test(base64_parse_2_chars, true) :-
		base64::parse(chars(['V','G',h,l,'I','H','F','1',a,'W','N',r,'I','G','J',y,b,'3',d,u,'I','G','Z',v,e,'C','B',q,d,'W','1',w,c,y,'B',v,d,m,'V',y,'I','H','R',o,'Z','S','B',s,'Y','X',p,'5','I','G','R',v,'Z',y,'4',=]), Bytes),
		^^assertion(atom_codes('The quick brown fox jumps over the lazy dog.', Bytes)).

	test(base64_parse_2_codes, true) :-
		base64::parse(codes([86,71,104,108,73,72,70,49,97,87,78,114,73,71,74,121,98,51,100,117,73,71,90,118,101,67,66,113,100,87,49,119,99,121,66,118,100,109,86,121,73,72,82,111,90,83,66,115,89,88,112,53,73,71,82,118,90,121,52,61]), Bytes),
		^^assertion(atom_codes('The quick brown fox jumps over the lazy dog.', Bytes)).

	test(base64_generate_2_01, true) :-
		atom_codes('The quick brown fox jumps over the lazy dog.', Bytes),
		base64::generate(atom(Base64), Bytes),
		^^assertion(Base64 == 'VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=').

	test(base64_generate_2_02, true) :-
		atom_codes('The quick brown fox jumps over the lazy dog.', Bytes),
		base64::generate(chars(Base64), Bytes),
		^^assertion(Base64 == ['V','G',h,l,'I','H','F','1',a,'W','N',r,'I','G','J',y,b,'3',d,u,'I','G','Z',v,e,'C','B',q,d,'W','1',w,c,y,'B',v,d,m,'V',y,'I','H','R',o,'Z','S','B',s,'Y','X',p,'5','I','G','R',v,'Z',y,'4',=]).

	test(base64_generate_2_03, true) :-
		atom_codes('The quick brown fox jumps over the lazy dog.', Bytes),
		base64::generate(codes(Base64), Bytes),
		^^assertion(Base64 == [86,71,104,108,73,72,70,49,97,87,78,114,73,71,74,121,98,51,100,117,73,71,90,118,101,67,66,113,100,87,49,119,99,121,66,118,100,109,86,121,73,72,82,111,90,83,66,115,89,88,112,53,73,71,82,118,90,121,52,61]).

	test(base64_roundtrip_01, true) :-
		this(This),
		object_property(This, file(_, Directory)),
		os::path_concat(Directory, 'test_files/logtalk.png', Path),
		reader::file_to_bytes(Path, Bytes0),
		base64::generate(codes(Codes), Bytes0),
		base64::parse(codes(Codes), Bytes),
		^^assertion(Bytes == Bytes0).

	test(base64_roundtrip_02, true) :-
		this(This),
		object_property(This, file(_, Directory)),
		os::path_concat(Directory, 'test_files/dump_base64.txt', Path),
		atom_codes('The quick brown fox jumps over the lazy dog.', Bytes0),
		base64::generate(file(Path), Bytes0),
		base64::parse(file(Path), Bytes),
		^^assertion(Bytes == Bytes0).

	test(base64url_parse_atom, true) :-
		base64url::parse(atom('aHR0cHM6Ly9sb2d0YWxrLm9yZw'), URL),
		^^assertion(URL == 'https://logtalk.org').

	test(base64url_parse_chars, true) :-
		base64url::parse(chars([a,'H','R','0',c,'H','M','6','L',y,'9',s,b,'2',d,'0','Y','W',x,r,'L',m,'9',y,'Z',w]), URL),
		^^assertion(URL == [h,t,t,p,s,:,/,/,l,o,g,t,a,l,k,'.',o,r,g]).

	test(base64url_parse_codes, true) :-
		base64url::parse(codes([97,72,82,48,99,72,77,54,76,121,57,115,98,50,100,48,89,87,120,114,76,109,57,121,90,119]), URL),
		^^assertion(URL == [104,116,116,112,115,58,47,47,108,111,103,116,97,108,107,46,111,114,103]).

	test(base64url_generate_atom, true) :-
		base64url::generate(atom(Base64URL), 'https://logtalk.org'),
		^^assertion(Base64URL == 'aHR0cHM6Ly9sb2d0YWxrLm9yZw').

	test(base64url_generate_chars, true) :-
		base64url::generate(chars(Base64URL), [h,t,t,p,s,:,/,/,l,o,g,t,a,l,k,'.',o,r,g]),
		^^assertion(Base64URL == [a,'H','R','0',c,'H','M','6','L',y,'9',s,b,'2',d,'0','Y','W',x,r,'L',m,'9',y,'Z',w]).

	test(base64url_generate_codes, true) :-
		base64url::generate(codes(Base64URL), [104,116,116,112,115,58,47,47,108,111,103,116,97,108,107,46,111,114,103]),
		^^assertion(Base64URL == [97,72,82,48,99,72,77,54,76,121,57,115,98,50,100,48,89,87,120,114,76,109,57,121,90,119]).

:- end_object.
