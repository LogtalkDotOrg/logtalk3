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


:- object(test_hook_object,
	implements(expanding)).

	term_expansion(a(X), a(Y)) :- Y is X + 1.
	term_expansion(b(X), b(Y)) :- Y is X + 1.
	term_expansion(c(X), c(Y)) :- Y is X + 1.
	term_expansion(d(X), d(Y)) :- Y is X + 1.
	term_expansion(e(X), e(Y)) :- Y is X + 1.
	term_expansion(f(X), f(Y)) :- Y is X + 1.

	goal_expansion(true, fail).

:- end_object.
