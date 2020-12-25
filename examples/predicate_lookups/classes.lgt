%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% using classes require at least the definition of a metaclass

:- object(meta,
	% avoid infinite regression by having the metaclass instantiate itself
	instantiates(meta)).

:- end_object.


:- object(native,
	instantiates(meta)).

:- end_object.


:- object(artificial,
	instantiates(meta)).

	:- public(purpose/1).

:- end_object.


:- object(aircraft,
	specializes(artificial)).

	:- public(structure/1).
	% by default, assume the aircraft have a rigid structure
	structure(rigid).

	% by default, assume the purpose of an aircraft is transport
	purpose(transport).

:- end_object.


:- object(paraglider,
	instantiates(aircraft)).

	% override inherited definition
	purpose(fun).

	% override inherited definition
	structure(soft).

:- end_object.


:- object(sailplane,
	instantiates(aircraft)).

	% override inherited definition
	purpose(fun).

:- end_object.
