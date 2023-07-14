%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- set_logtalk_flag(singleton_variables, silent).
:- set_logtalk_flag(unknown_entities, silent).


:- object(sample(_PV1_, _PV2_)).

	a(A,B,C,B,A).

:- end_object.


:- protocol(ptc1).

:- end_protocol.


:- protocol(ptc2,
	extends(ptc1)).

:- end_protocol.


:- category(ctg2,
	implements(ptc1),
	extends(protected::ctg1)).

:- end_category.


:- object(obj1,
	implements((ptc1, ptc2)),
	imports(ctg1),
	extends(private::obj2)).

:- end_object.


:- object(obj3,
	implements(ptc2),
	imports(private::ctg2),
	instantiates(obj4),
	specializes(obj5)).

	:- set_logtalk_flag(complements, allow).

:- end_object.


:- category(ctg3,
	complements(obj3)).

:- end_category.
