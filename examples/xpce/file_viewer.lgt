%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


% straightforward adaptation of an example from the XPCE user manual


:- object(view).

	:- include('xpce_includes.lgt').

	:- public(file_viewer/1).
	file_viewer(Dir) :-
		new(DirObj, directory(Dir)),
		new(F, frame('File Viewer')),
		send(F, append(new(B, browser))),
		send(new(D, dialog), below(B)),
		send(D, append(button(view, logtalk(controller, action, DirObj, B?selection?key)))),
		send(D, append(button(quit, message(F, destroy)))),
		send(B, members(DirObj?files)),
		send(F, open).

:- end_object.


:- object(controller).

	:- include('xpce_includes.lgt').

	:- public(action/2).
	action(DirObj, F) :-
		send(new(V, view(F)), open),
		get(DirObj, file(F), FileObj),
		send(V, load(FileObj)).

:- end_object.
