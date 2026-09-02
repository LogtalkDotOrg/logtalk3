:- encoding('UTF-8').


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


	test(uri_template_expand_3_19, deterministic(Expansion == "%C3%A9")) :-
		uri_template::expand("{value}", ["value"-string("é")], Expansion).

	test(uri_template_expand_3_28, deterministic(Expansion == "%C3%A9")) :-
		uri_template::expand("{+value:1}", ["value"-string("%C3%A9clair")], Expansion).

	test(uri_template_expand_3_29, deterministic(Expansion == "%C3%A9")) :-
		uri_template::expand("{value:1}", ["value"-string("éclair")], Expansion).
