%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(library(basic_types_loader)),
	logtalk_load(library(os_loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load(main),
	% test with loading from a relative path a file that uses include/1
	% directives with relative paths that go up to the file parent directory
	logtalk_load('down/up'),
	% test with loading from an absolute path a file that uses include/1
	% directives with relative paths that go into sub-directories
	logtalk_load('$LOGTALKUSER/tests/logtalk/directives/include_1/sub'),
	logtalk_load(tests, [hook(lgtunit), source_data(on)]),
	tests::run
)).
