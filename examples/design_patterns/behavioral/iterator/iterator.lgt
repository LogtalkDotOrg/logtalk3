%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Iterator design pattern:
%
% https://en.wikipedia.org/wiki/Iterator_pattern


% we can define a simple protocol with an iterator predicate

:- protocol(iterating).

	:- public(element/1).

:- end_protocol.


% the protocol can then be implemented by any object representing
% a collection

:- object(book_collection,
	implements(iterating)).

	:- public(add_title/1).
	add_title(Title) :-
		assertz(title_(Title)).

	% the iterator hides the internal representation
	element(Title) :-
		title_(Title).

	:- private(title_/1).
	:- dynamic(title_/1).

:- end_object.
