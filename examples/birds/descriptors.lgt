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


:- category(descriptors).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/2/18,
		comment is 'Bird descriptors predicates.',
		source is 'Example adapted from an Amzi! Inc Prolog book.'
	]).

	:- public([
		bill/1,
		cheek/1,
		color/1,
		eats/1,
		family/1,
		feed/1,
		feet/1,
		flight/1,
		flight_profile/1,
		head/1,
		live/1,
		neck/1,
		nostrils/1,
		order/1,
		size/1,
		tail/1,
		throat/1,
		voice/1,
		wings/1]).

	:- public(descriptor/1).

	descriptor(bill/1).
	descriptor(cheek/1).
	descriptor(color/1).
	descriptor(eats/1).
	descriptor(feed/1).
	descriptor(feet/1).
	descriptor(flight/1).
	descriptor(flight_profile/1).
	descriptor(head/1).
	descriptor(live/1).
	descriptor(neck/1).
	descriptor(nostrils/1).
	descriptor(size/1).
	descriptor(tail/1).
	descriptor(throat/1).
	descriptor(voice/1).
	descriptor(wings/1).

:- end_category.
