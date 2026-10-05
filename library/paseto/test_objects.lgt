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


:- object(paseto_v4_test_driver,
	extends(paseto_v4)).

	:- public(deterministic_local_encrypt/6).
	:- mode(deterministic_local_encrypt(+list(byte), +list(byte), +list(byte), +list(byte), +list(byte), -atom), one_or_error).
	:- info(deterministic_local_encrypt/6, [
		comment is 'Calls the protected explicit-nonce encryption predicate for conformance tests.',
		argnames is ['Key', 'Nonce', 'Payload', 'Footer', 'ImplicitAssertion', 'Token']
	]).

	deterministic_local_encrypt(Key, Nonce, Payload, Footer, ImplicitAssertion, Token) :-
		^^local_encrypt_with_nonce(Key, Nonce, Payload, Footer, ImplicitAssertion, Token).

	:- public(test_pae/2).
	:- mode(test_pae(+list(list(byte)), -list(byte)), one).
	:- info(test_pae/2, [
		comment is 'Exposes PAE for conformance tests.',
		argnames is ['Pieces', 'Encoding']
	]).

	test_pae(Pieces, Encoding) :-
		^^pae(Pieces, Encoding).

:- end_object.
