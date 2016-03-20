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


:- protocol(test_protocol).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/02/08,
		comment is 'Sample protocol for testing with the `source_data` flag turned on.']).

	:- public(a/1).
	:- if(current_logtalk_flag(coinduction, supported)).
		:- coinductive(a/1).
	:- endif.

	:- protected(b/2).
	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized(b/2).
	:- endif.

	:- private(c/3).
	:- dynamic(c/3).

:- end_protocol.


:- protocol(empty_protocol).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/02/19,
		comment is 'Empty protocol for testing validity of protocol properties.']).

:- end_protocol.


:- protocol(built_in_protocol).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Built-in protocol for testing determinism of protocol properties.']).

	:- built_in.

:- end_protocol.


:- protocol(dynamic_protocol).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Dynamic protocol for testing determinism of protocol properties.']).

	:- (dynamic).

:- end_protocol.


:- protocol(debug_protocol).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/03/20,
		comment is 'Protocol compiled in debug mode for testing determinism of protocol properties.']).

	:- set_logtalk_flag(debug, on).

:- end_protocol.
