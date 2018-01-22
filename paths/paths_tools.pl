%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths
%  Last updated on October 24, 2017
%
%  This file is part of Logtalk <https://logtalk.org/>
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

% developer tools

logtalk_library_path(tools, logtalk_user('tools/')).

logtalk_library_path(assertions, tools('assertions/')).
logtalk_library_path(code_metrics, tools('code_metrics/')).
logtalk_library_path(dead_code_scanner, tools('dead_code_scanner/')).
logtalk_library_path(debugger, tools('debugger/')).
logtalk_library_path(diagrams, tools('diagrams/')).
logtalk_library_path(doclet, tools('doclet/')).
logtalk_library_path(help, tools('help/')).
logtalk_library_path(lgtdoc, tools('lgtdoc/')).
logtalk_library_path(lgtunit, tools('lgtunit/')).
logtalk_library_path(ports, tools('ports/')).
logtalk_library_path(profiler, tools('profiler/')).
logtalk_library_path(wrapper, tools('wrapper/')).
