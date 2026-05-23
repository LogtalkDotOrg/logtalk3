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


:- object(http_octet_stream_body_codec,
	implements(http_body_codec_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Concrete HTTP body codec for octet-stream payloads.'
	]).

	:- uses(type, [
		valid/2
	]).

	media_types(['application/octet-stream']).

	encode_body(MediaType, binary(Bytes), _Options, content(MediaType, binary(Bytes))) :-
		valid(list(byte), Bytes).
	encode_body(MediaType, Bytes, _Options, content(MediaType, binary(Bytes))) :-
		valid(list(byte), Bytes).

	decode_body(MediaType, content(MediaType, binary(Bytes)), _Options, Bytes) :-
		valid(list(byte), Bytes).

:- end_object.
