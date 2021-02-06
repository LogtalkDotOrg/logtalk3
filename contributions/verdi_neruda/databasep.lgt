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


:- protocol(databasep).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'Database protocol.'
	]).

	:- public(rule/4).
	:- mode(rule(?callable, ?callable, -, -), zero_or_more).
	:- info(rule/4, [
		comment is 'Clauses for this predicate are automatically generated using term-expansion. The third argument contains the length of Body.',
		argnames is ['Head', 'Body', 'Length', 'Tail']
	]).

	:- public(rule/3).
	:- mode(rule(?callable, ?callable, -), zero_or_more).
	:- info(rule/3, [
		comment is 'Clauses for this predicate are automatically generated using term-expansion. The third argument denotes the tail of the Body.',
		argnames is ['Head', 'Body', 'Tail']
	]).

	:- public(rule/2).
	:- mode(rule(?callable, -list(callable)), zero_or_more).
	:- info(rule/2, [
		comment is 'Clauses for this predicate are automatically generated using term-expansion.',
		argnames is ['Head', 'Body']
	]).

	:- public(bench_goal/1).
	:- mode(bench_goal(?callable), zero_or_more).
	:- info(bench_goal/1, [
		comment is 'Table of benchmark goals. They are used from shell.lgt to make benchmarking easier.',
		argnames is ['Goal']
	]).

:- end_protocol.
