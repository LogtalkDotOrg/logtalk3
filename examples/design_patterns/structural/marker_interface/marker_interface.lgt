%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


% first, define a marker interface, which usually is empty

:- protocol(marker).

:- end_protocol.


% the marker interface can be "implemented" by objects and
% categories and inherited by descendant objects and objects
% that import those categories

:- category(a_category,
	implements(marker)).

:- end_category.


:- object(an_object,
	implements(marker)).

:- end_object.


% descendant objects inherit the marker interface

:- object(a_descendant_object,
	extends(an_object)).

:- end_object.


% same for objects implementing the category

:- object(another_object,
	imports(a_category)).

:- end_object.
