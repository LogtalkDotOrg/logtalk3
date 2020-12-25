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


% classes and instances

:- object(mc, instantiates(mc), imports(class_hierarchy)).
:- end_object.


:- object(c1, instantiates(mc), imports(class_hierarchy)).
:- end_object.

:- object(c2, instantiates(mc), imports(class_hierarchy)).
:- end_object.


:- object(sc1, instantiates(mc), specializes(c1)).
:- end_object.

:- object(sc2, instantiates(mc), specializes(c1)).
:- end_object.

:- object(sc3, instantiates(mc), specializes([c1,c2])).
:- end_object.


:- object(i1, instantiates(sc1)).
:- end_object.

:- object(i2, instantiates(sc1)).
:- end_object.

:- object(i3, instantiates([sc1,sc3])).
:- end_object.


% prototypes

:- object(r, imports(proto_hierarchy)).
:- end_object.


:- object(p1, extends(r)).
:- end_object.

:- object(p2, extends(r)).
:- end_object.

:- object(p3, extends(r)).
:- end_object.


:- object(d1, extends(p1)).
:- end_object.

:- object(d2, extends(p1)).
:- end_object.

:- object(d3, extends([p1,p2])).
:- end_object.
