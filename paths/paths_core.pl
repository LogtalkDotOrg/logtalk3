%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths
%  Last updated on October 24, 2017
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


% logtalk_library_path(Library, Path)
%
% paths must always end with a "/"

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

% when the LOGTALKHOME or the LOGTALKUSER environment variables are not
% defined (we may be e.g. embedding Logtalk in a compiled application),
% assume the current directory as their value

:- initialization((
	% Logtalk startup directory
	(	'$lgt_environment_variable'('LOGTALK_STARTUP_DIRECTORY', _) ->
		LOGTALK_STARTUP_DIRECTORY = '$LOGTALK_STARTUP_DIRECTORY/'
	;	'$lgt_current_directory'(LOGTALK_STARTUP_DIRECTORY0),
		(	sub_atom(LOGTALK_STARTUP_DIRECTORY0, _, _, 0, '/') ->
			LOGTALK_STARTUP_DIRECTORY = LOGTALK_STARTUP_DIRECTORY0
		;	atom_concat(LOGTALK_STARTUP_DIRECTORY0, '/', LOGTALK_STARTUP_DIRECTORY)
		)
	),
	assertz(logtalk_library_path(startup, LOGTALK_STARTUP_DIRECTORY)),
	% Logtalk installation directory
	(	'$lgt_environment_variable'('LOGTALKHOME', _) ->
		LOGTALKHOME = '$LOGTALKHOME/'
	;	'$lgt_current_directory'(LOGTALKHOME0),
		(	sub_atom(LOGTALKHOME0, _, _, 0, '/') ->
			LOGTALKHOME = LOGTALKHOME0
		;	atom_concat(LOGTALKHOME0, '/', LOGTALKHOME)
		)
	),
	assertz(logtalk_library_path(logtalk_home, LOGTALKHOME)),
	% Logtalk user directory
	(	'$lgt_environment_variable'('LOGTALKUSER', _) ->
		LOGTALKUSER = '$LOGTALKUSER/'
	;	'$lgt_current_directory'(LOGTALKUSER0),
		(	sub_atom(LOGTALKUSER0, _, _, 0, '/') ->
			LOGTALKUSER = LOGTALKUSER0
		;	atom_concat(LOGTALKUSER0, '/', LOGTALKUSER)
		)
	),
	assertz(logtalk_library_path(logtalk_user, LOGTALKUSER))
)).

% user home directory
logtalk_library_path(home, HOME) :-
	(	'$lgt_environment_variable'('HOME', _) ->
		% likely a POSIX system but Windows users
		% may also define this environment variable
		HOME = '$HOME/'
	;	'$lgt_environment_variable'('USERPROFILE', _) ->
		% Windows systems define this environment variable
		HOME = '$USERPROFILE/'
	;	fail
	).

% core library, required for Logtalk startup
logtalk_library_path(core, logtalk_home('core/')).

% main directories in the Logtalk distribution not defined elsewhere
logtalk_library_path(coding, logtalk_user('coding/')).
logtalk_library_path(library, logtalk_user('library/')).
