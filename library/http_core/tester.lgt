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


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(basic_types(loader)),
	logtalk_load(os(loader)),
	logtalk_load(reader(loader)),
	logtalk_load(base64(loader)),
	logtalk_load(character_sets(loader)),
	logtalk_load(json(loader)),
	logtalk_load(url(loader)),
	logtalk_load(http_cookies(loader)),
	logtalk_load([
		http_request_protocol,
		http_response_protocol,
		http_body_codec_protocol,
		http_handler_protocol,
		http_json_term_helpers,
		http_text_helpers,
		http_octet_stream_body_codec,
		http_text_body_codec,
		http_json_body_codec,
		http_form_body_codec,
		http_core,
		http_origin_site_helpers,
		http_docroot_paths,
		http_message_helpers
	], [
		debug(on),
		source_data(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
