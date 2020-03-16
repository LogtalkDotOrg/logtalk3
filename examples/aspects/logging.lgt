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


:- object(logging,
	implements(monitoring)).

	% watch all "before" and "after" events for bank::transfer/3 messages
	:- initialization(define_events(_, bank, transfer(_,_,_), _, logging)).

	% print logging messages for all transfers

	before(bank, transfer(From, To, Amount), _) :-
		write('Attempting transfer:'), nl,
		write('  From:   '), write(From), nl,
		write('  To:     '), write(To), nl,
		write('  Amount: '), write(Amount), nl.

	after(bank, transfer(_, _, _), _) :-
		write('Transfer successful.'), nl.

:- end_object.
