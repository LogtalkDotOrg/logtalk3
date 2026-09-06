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
	logtalk_load(strings(loader), [optimize(on)]),
	logtalk_load(unicode_data(unicode_character_data), [reload(skip)]),
	logtalk_load([
		text_normalization_profile_protocol,
		'data/xml_whatwg_entities',
		unicode_data('test_files/unicode_normalization_test_data'),
		unicode,
		case_folding,
		'profiles/default_text_normalization'
	], [
		optimize(on)
	]),
	logtalk_load(lgtunit(loader), [optimize(on)]),
	logtalk_load(tests_unicode_conformance, [optimize(on), hook(lgtunit)]),
	tests_unicode_conformance::run
)).
