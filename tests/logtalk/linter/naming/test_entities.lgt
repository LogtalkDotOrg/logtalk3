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


:- object(naming).

	:- public(fooBar/0).
	:- public(foo42bar/0).

	:- private(nonTerminal//0).
	:- private(non42terminal//0).

	bazQux.

	baz42qux.

	bar(Corge42Grault, Corge42Grault).

	quux(Corge_Grault, Corge_Grault).

	noMoreTokens --> eos.

	no42tokens --> eos.

	predicate(List, LIST, List, LIST).

:- end_object.



:- object(someObject).

:- end_object.



:- protocol(foo42bar).

:- end_protocol.
