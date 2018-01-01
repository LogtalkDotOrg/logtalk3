%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(datep).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/3/17,
		comment is 'Date protocol.'
	]).

	:- public(today/3).
	:- mode(today(-integer, -integer, -integer), one).
	:- info(today/3, [
		comment is 'Returns current date.',
		argnames is ['Year', 'Month', 'Day']
	]).

	:- public(leap_year/1).
	:- mode(leap_year(+integer), zero_or_one).
	:- info(leap_year/1, [
		comment is 'True if the argument is a leap year.',
		argnames is ['Year']
	]).

	:- public(name_of_day/3).
	:- mode(name_of_day(?integer, ?atom, ?atom), zero_or_more).
	:- info(name_of_day/3, [
		comment is 'Name and short name of day.',
		argnames is ['Index', 'Name', 'Short']
	]).

	:- public(name_of_month/3).
	:- mode(name_of_month(?integer, ?atom, ?atom), zero_or_more).
	:- info(name_of_month/3, [
		comment is 'Name and short name of month.',
		argnames is ['Index', 'Name', 'Short']
	]).

	:- public(days_in_month/3).
	:- mode(days_in_month(?integer, +integer, ?integer), zero_or_more).
	:- info(days_in_month/3, [
		comment is 'Number of days in a month.',
		argnames is ['Month', 'Year', 'Days']
	]).

	:- public(valid/3).
	:- mode(valid(@integer, @integer, @integer), zero_or_one).
	:- info(valid/3, [
		comment is 'True if the arguments represent a valid date.',
		argnames is ['Year', 'Month', 'Day']
	]).

:- end_protocol.
