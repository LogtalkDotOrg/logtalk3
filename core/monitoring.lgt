%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(monitoring).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/04,
		comment is 'Event handlers protocol. The handlers are automatically called for messages sent using the ::/2 control construct from within objects compiled with the "events" flag set to "allow".'
	]).

	:- built_in.

	:- public(before/3).
	:- mode(before(@term, @term, @term), zero_or_one).
	:- info(before/3, [
		comment is 'Event handler for "before" events. A "before" event handler may prevent a method from being looked up or called by failing.',
		argnames is ['Object', 'Message', 'Sender']
	]).

	:- public(after/3).
	:- mode(after(@term, @term, @term), zero_or_one).
	:- info(after/3, [
		comment is 'Event handler for "after" events. An "after" event handler may prevent a method from succeeding by failing.',
		argnames is ['Object', 'Message', 'Sender']
	]).

:- end_protocol.
