%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Sample loader file
%  Last updated on April 26, 2017
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


%  This is a sample loader file. Loader files are simply Logtalk source
%  files whose main purpose is to load your application files.
%
%  During development, loader files can be reloaded repeatedly. Therefore,
%  any generic settings shared by all source files, such as library paths,
%  global flag values, and initialization goals with side-effects, may be
%  defined in alternative in a settings file saved in the application
%  directory and by starting Logtalk from that directory.

%  If you need to preload plain Prolog files or Prolog module files (e.g.
%  because those resources are used in the Logtalk code), do so preferably
%  using standard ensure_loaded/1 or use_module/1-2 directives. For example:

:- ensure_loaded(prolog_source_file).
:- use_module(prolog_module_source_file, []).

%  Load your application source files using calls to the logtalk_load/1-2
%  built-in predicates but wrapping them in initialization/1 directives to
%  ensure portability across backend Prolog systems:

:- initialization((
	logtalk_load([
		logtalk_source_file_1,
		logtalk_source_file_2,
		...
	], [
		% required compiler options
	])
)).

%  Multiple initialization/1 directives can be used when necessary; their
%  goals will be called in order.

%  Tip: don't use debug/1 or optimize/1 compiler options in `logtalk_load/2`
%  calls to allow easy recompilation of source files for debugging during
%  development. Later, when the code becomes stable, you can always add a
%  `optimize(on)` compiler option for best performance.
