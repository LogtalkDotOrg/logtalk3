%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths for the user installed packs
%  Last updated on October 15, 2021
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

% packs (third-party libraries) directory

logtalk_library_path(logtalk_packs, LogtalkPacks) :-
	(	'$lgt_environment_variable'('LOGTALKPACKS', _) ->
		LogtalkPacks = '$LOGTALKPACKS/'
	;	LogtalkPacks = home('logtalk_packs/')
	).
logtalk_library_path(Pack, logtalk_packs(Path)) :-
	(	'$lgt_environment_variable'('LOGTALKPACKS', _) ->
		PathsPacks = '$LOGTALKPACKS/packs'
	;	'$lgt_environment_variable'('HOME', _) ->
		PathsPacks = '$HOME/logtalk_packs/packs'
	;	'$lgt_environment_variable'('USERPROFILE', _) ->
		PathsPacks = '$USERPROFILE/logtalk_packs/packs'
	;	fail
	),
	'$lgt_expand_path'(PathsPacks, ExpandedPath),
	'$lgt_directory_exists'(ExpandedPath),
	(	var(Pack) ->
		'$lgt_directory_files'(ExpandedPath, Files),
		'$lgt_member'(Pack, Files),
		\+ sub_atom(Pack, 0, _, _, '.')
	;	true
	),
	atom_concat(ExpandedPath, '/', PackPath0),
	atom_concat(PackPath0, Pack, PackPath),
	'$lgt_directory_exists'(PackPath),
	atom_concat('packs/', Pack, Path0),
	atom_concat(Path0, '/', Path).
