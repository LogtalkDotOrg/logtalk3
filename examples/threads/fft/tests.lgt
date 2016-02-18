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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/25,
		comment is 'Unit tests for the "threads/fft" example.'
	]).

	cover(fft(_)).

	test(fft_1) :-
		N is 8, cgenerator::list(N,L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F4).

	test(fft_2) :-
		N is 8, L = [c(1,0),c(2,0),c(3,0),c(4,0),c(5,0),c(6,0),c(7,0),c(8,0)],
		fft(4)::fft(N,L,_F4), fft(8)::fft(N,L,_F8).

	test(fft_3) :-
		N is 16384, cgenerator::list(N, L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F3).

	test(fft_4) :-
		N is 8192, cgenerator::list(N, L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F3).

	test(fft_5) :-
		N is 65536, cgenerator::list(N, L),
		fft(1)::fft(N,L,_F1), fft(2)::fft(N,L,_F2), fft(4)::fft(N,L,_F3).

:- end_object.
