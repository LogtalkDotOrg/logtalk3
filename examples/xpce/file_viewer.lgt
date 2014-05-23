%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
