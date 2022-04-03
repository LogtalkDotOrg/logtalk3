%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Loader file for all installed packs
%  Last updated on April 3, 2022
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- initialization(
	forall(
		pack_loader_file(PackLoader),
		logtalk_load(PackLoader)
	)
).

pack_loader_file(PackLoader) :-
	(	'$lgt_expand_library_alias'(logtalk_packs, LogtalkPacks) ->
		atom_concat(LogtalkPacks, '/packs', PathsPacks)
	;	'$lgt_environment_variable'('LOGTALKPACKS', _) ->
		PathsPacks = '$LOGTALKPACKS/packs'
	;	'$lgt_environment_variable'('HOME', _) ->
		PathsPacks = '$HOME/logtalk_packs/packs'
	;	'$lgt_environment_variable'('USERPROFILE', _) ->
		PathsPacks = '$USERPROFILE/logtalk_packs/packs'
	;	fail
	),
	'$lgt_expand_path'(PathsPacks, ExpandedPath),
	'$lgt_directory_exists'(ExpandedPath),
	'$lgt_directory_files'(ExpandedPath, Files),
	'$lgt_member'(Pack, Files),
	\+ sub_atom(Pack, 0, _, _, '.'),
	(	atomic_list_concat([ExpandedPath, '/', Pack, '/loader.lgt'], PackLoader),
		'$lgt_file_exists'(PackLoader) ->	
		true
	;	atomic_list_concat([ExpandedPath, '/', Pack, '/loader.logtalk'], PackLoader),
		'$lgt_file_exists'(PackLoader)
	).
