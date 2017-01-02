%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for SWI-Prolog
%  Last updated on July 10, 2014
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


% load Logtalk core files
:-	getenv('LOGTALKHOME', LogtalkHome0),
	prolog_to_os_filename(LogtalkHome, LogtalkHome0),
	atom_concat(LogtalkHome, '/adapters/swi.pl', AdapterFile), consult(AdapterFile),
	atom_concat(LogtalkHome, '/paths/paths.pl', PathsFile), consult(PathsFile),
	atom_concat(LogtalkHome, '/integration/logtalk_comp_swi.pl', IntegrationFile), consult(IntegrationFile),
	atom_concat(LogtalkHome, '/adapters/swihooks.pl', HooksFile), consult(HooksFile).
