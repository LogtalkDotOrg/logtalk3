%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- set_logtalk_flag(source_data, on).


:- category(test_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/12/17,
		comment is 'Sample category for testing with the `source_data` flag turned on.']).

	:- public(a/1).
	:- if(current_logtalk_flag(coinduction, supported)).
		:- coinductive(a/1).
	:- endif.
	a(1).

	:- protected(b/2).
	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized(b/2).
	:- endif.
	b(1, 2).
	b(2, 1).

	:- private(c/3).
	:- dynamic(c/3).

	d(1, 2, 3, 4).
	d(2, 3, 4, 1).
	d(3, 4, 1, 2).
	d(4, 1, 2, 3).

	:- private(e/5).

:- end_category.


:- category(empty_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/02/19,
		comment is 'Empty category for testing validity of category properties.'
	]).

:- end_category.


:- category(built_in_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Built-in category for testing determinism of category properties.']).

	:- built_in.

:- end_category.


:- category(dynamic_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Dynamic category for testing determinism of category properties.']).

	:- (dynamic).

:- end_category.


:- category(debug_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Category compiled in debug mode for testing determinism of category properties.']).

	:- set_logtalk_flag(debug, on).

:- end_category.


:- category(events_category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Category compiled with event support for testing determinism of category properties.']).

	:- set_logtalk_flag(events, allow).

:- end_category.
