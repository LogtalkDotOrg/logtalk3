%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths for core resources
%  Last updated on April 12, 2018
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% logtalk_library_path(Library, Path)
%
% paths must always end with a "/"

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

% Logtalk startup directory
:- initialization((
	(	'$lgt_environment_variable'('LOGTALK_STARTUP_DIRECTORY', _) ->
		LOGTALK_STARTUP_DIRECTORY = '$LOGTALK_STARTUP_DIRECTORY/'
	;	'$lgt_current_directory'(LOGTALK_STARTUP_DIRECTORY0),
		(	sub_atom(LOGTALK_STARTUP_DIRECTORY0, _, _, 0, '/') ->
			LOGTALK_STARTUP_DIRECTORY = LOGTALK_STARTUP_DIRECTORY0
		;	atom_concat(LOGTALK_STARTUP_DIRECTORY0, '/', LOGTALK_STARTUP_DIRECTORY)
		)
	),
	assertz(logtalk_library_path(startup, LOGTALK_STARTUP_DIRECTORY))
)).

% Logtalk installation directory
logtalk_library_path(logtalk_home, '$LOGTALKHOME/').

% Logtalk user directory
logtalk_library_path(logtalk_user, '$LOGTALKUSER/').

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
