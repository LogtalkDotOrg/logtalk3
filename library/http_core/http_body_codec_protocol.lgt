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


:- protocol(http_body_codec_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Protocol for portable HTTP body codecs.'
	]).

	:- public(media_types/1).
	:- mode(media_types(-list(atom)), one).
	:- info(media_types/1, [
		comment is 'Returns the media types supported by this body codec.',
		argnames is ['MediaTypes']
	]).

	:- public(encode_body/4).
	:- mode(encode_body(+atom, ++term, +list(compound), -compound), one_or_error).
	:- info(encode_body/4, [
		comment is 'Encodes a semantic payload term for the given media type and options into a normalized HTTP body term.',
		argnames is ['MediaType', 'Payload', 'Options', 'Body']
	]).

	:- public(decode_body/4).
	:- mode(decode_body(+atom, ++compound, +list(compound), -term), one_or_error).
	:- info(decode_body/4, [
		comment is 'Decodes a normalized HTTP body term for the given media type and options into a semantic payload term.',
		argnames is ['MediaType', 'Body', 'Options', 'Payload']
	]).

:- end_protocol.
