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
	logtalk_load(basic_types(loader)),
	logtalk_load(reader(loader)),
	logtalk_load(base64(loader)),
	logtalk_load(json(loader)),
	logtalk_load(url(loader)),
	logtalk_load(hashes(loader)),
	logtalk_load('../http_cookies/loader.lgt'),
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
		http,
		http_message_helpers
	], [
		optimize(on)
	])
)).
