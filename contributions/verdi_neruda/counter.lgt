%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright (c) 2010, Victor Lagerkvist
%  SPDX-License-Identifier: BSD-3-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(counter).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'Counter implemented with asserta/retract.'
	]).

	:- initialization(init).

	:- public(increment/0).
	:- mode(increment, one).
	:- info(increment/0, [
		comment is 'Increment the counter by 1.'
	]).

	:- public(increase/1).
	:- mode(increase(+number), one).
	:- info(increase/1, [
		comment is 'Increments the counter by the specified amount.',
		argname is ['I']
	]).

	:- public(set/1).
	:- mode(set(+number), one).
	:- info(set/1, [
		comment is 'Sets the counter to the specified amount.',
		argname is ['N']
	]).

	:- public(value/1).
	:- mode(value(?number), one).
	:- info(value/1, [
		comment is 'Gets the current value of the counter.',
		argname is ['N']
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets the counter to zero.'
	]).

	:- private(c/1).
	:- dynamic(c/1).

	init :-
		retractall(c(_)),
		asserta(c(0)).

	increment :-
		retract(c(N0)),
		N is N0 + 1,
		asserta(c(N)).

	increase(I) :-
		retract(c(N0)),
		N is N0 + I,
		asserta(c(N)).

	set(N) :-
		retract(c(_)),
		asserta(c(N)).

	value(N) :-
		c(N).

	reset :-
		retract(c(_)),
		asserta(c(0)).

:- end_object.
