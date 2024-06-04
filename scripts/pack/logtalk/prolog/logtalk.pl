%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Support for load-on-demand using SWI Prolog 6.6.0 and later versions
%  Last updated on February 14, 2016
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- module(logtalk, []).

:-	prolog_load_context(directory, Directory),
	atom_concat(Directory, '/../logtalk-3.81.0/', Location),
	setenv('LOGTALKHOME', Location),
	setenv('LOGTALKUSER', Location),
	user:load_files('../logtalk-3.81.0/integration/logtalk_swi.pl').
