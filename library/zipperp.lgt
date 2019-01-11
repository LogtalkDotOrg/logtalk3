%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(zipperp).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2019/01/11,
		comment is 'Zipper protocol.',
		see_also is [zlist]
	]).

	:- public(zip/2).
	:- mode(zip(+sequence, --zipper), zero_or_one).
	:- info(zip/2, [
		comment is 'Adds a zipper to a compound term holding a sequence of elements. Fails if the sequence is empty.',
		argnames is ['Sequence', 'Zipper']
	]).

	:- public(unzip/2).
	:- mode(unzip(@zipper, --sequence), one).
	:- info(unzip/2, [
		comment is 'Removes a zipper from a sequence.',
		argnames is ['Zipper', 'Sequence']
	]).

	:- public(current/2).
	:- mode(current(+zipper, ?term), zero_or_one).
	:- info(current/2, [
		comment is 'Current element.',
		argnames is ['Zipper', 'Current']
	]).

	:- public(next/2).
	:- mode(next(+zipper, --zipper), zero_or_one).
	:- info(next/2, [
		comment is 'Moves to the next element. Fails if already at the last elements.',
		argnames is ['Zipper', 'NewZipper']
	]).

	:- public(next/3).
	:- mode(next(+zipper, --zipper, -term), zero_or_one).
	:- info(next/3, [
		comment is 'Moves to and returns the next element. Fails if already at the last elements.',
		argnames is ['Zipper', 'NewZipper', 'Next']
	]).

	:- public(previous/2).
	:- mode(previous(+zipper, --zipper), zero_or_one).
	:- info(previous/2, [
		comment is 'Moves to the previous element. Fails if already at the first elements.',
		argnames is ['Zipper', 'NewZipper']
	]).

	:- public(previous/3).
	:- mode(previous(+zipper, --zipper, -term), zero_or_one).
	:- info(previous/3, [
		comment is 'Moves to and returns the previous element. Fails if already at the first element.',
		argnames is ['Zipper', 'NewZipper', 'Previous']
	]).

	:- public(apply/2).
	:- meta_predicate(apply(1,*)).
	:- mode(apply(+callable, +zipper), zero_or_more).
	:- info(apply/2, [
		comment is 'Applies a closure to the current element.',
		argnames is ['Closure', 'Zipper']
	]).

	:- public(insert_before/3).
	:- mode(insert_before(+zipper, ?term, --zipper), zero_or_one).
	:- info(insert_before/3, [
		comment is 'Inserts an element before the current one.',
		argnames is ['Zipper', 'Element', 'NewZipper']
	]).

	:- public(insert_after/3).
	:- mode(insert_after(+zipper, ?term, --zipper), zero_or_one).
	:- info(insert_after/3, [
		comment is 'Inserts an element after the current one.',
		argnames is ['Zipper', 'Element', 'NewZipper']
	]).

	:- public(replace/3).
	:- mode(replace(+zipper, ?term, --zipper), one).
	:- info(replace/3, [
		comment is 'Replaces the current element with a new element.',
		argnames is ['Zipper', 'NewCurrent', 'NewZipper']
	]).

	:- public(delete_and_previous/2).
	:- mode(delete_and_previous(+zipper, --zipper), zero_or_one).
	:- info(delete_and_previous/2, [
		comment is 'Deletes the current element and moves to the previous element. Fails if no previous element exists.',
		argnames is ['Zipper', 'NewZipper']
	]).

	:- public(delete_and_next/2).
	:- mode(delete_and_next(+zipper, --zipper), zero_or_one).
	:- info(delete_and_next/2, [
		comment is 'Deletes the current element and moves to the next element. Fails if no next element exists.',
		argnames is ['Zipper', 'NewZipper']
	]).

	:- public(delete_and_unzip/2).
	:- mode(delete_and_unzip(+zipper, --sequence), one).
	:- info(delete_and_unzip/2, [
		comment is 'Deletes the current element and removes the zipper returning the resulting sequence.',
		argnames is ['Zipper', 'Sequence']
	]).

:- end_protocol.
