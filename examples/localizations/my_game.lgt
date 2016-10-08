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


:- object(my_game(_)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/10/12,
		comment is 'A simple example of supporting application localization in multiple languages.',
		parnames is ['CountryCode']
	]).

	% we use an object parameter to pass the country code
	% of the language to be used when printing messages

	:- public(banner/0).

	banner :-
		parameter(1, CountryCode),
		logtalk::print_message(comment, my_game(CountryCode), banner).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).
	logtalk::message_prefix_stream(comment, my_game(_), '>>> ', Stream) :-
		current_output(Stream).

:- end_object.
