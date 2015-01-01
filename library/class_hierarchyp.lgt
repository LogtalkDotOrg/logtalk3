%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- protocol(class_hierarchyp,
	extends(hierarchyp)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Class hierarchy protocol.'
	]).

	:- public(class/1).
	:- mode(class(?object), zero_or_more).
	:- info(class/1, [
		comment is 'Returns, by backtracking, all object classes.',
		argnames is ['Class']
	]).

	:- public(classes/1).
	:- mode(classes(-list), one).
	:- info(classes/1, [
		comment is 'List of all object classes.',
		argnames is ['Classes']
	]).

	:- public(instance/1).
	:- mode(instance(?object), zero_or_more).
	:- info(instance/1, [
		comment is 'Returns, by backtracking, all class instances.',
		argnames is ['Instance']
	]).

	:- public(instances/1).
	:- mode(instances(-list), one).
	:- info(instances/1, [
		comment is 'List of all class instances.',
		argnames is ['Instances']
	]).

	:- public(subclass/1).
	:- mode(subclass(?object), zero_or_more).
	:- info(subclass/1, [
		comment is 'Returns, by backtracking, all class subclasses.',
		argnames is ['Subclass']
	]).

	:- public(subclasses/1).
	:- mode(subclasses(-list), one).
	:- info(subclasses/1, [
		comment is 'List of all class subclasses.',
		argnames is ['Subclasses']
	]).

	:- public(superclass/1).
	:- mode(superclass(?object), zero_or_more).
	:- info(superclass/1, [
		comment is 'Returns, by backtracking, all class superclasses.',
		argnames is ['Superclass']
	]).

	:- public(superclasses/1).
	:- mode(superclasses(-list), one).
	:- info(superclasses/1, [
		comment is 'List of all class superclasses.',
		argnames is ['Superclasses']
	]).

	:- public(leaf_instance/1).
	:- mode(leaf_instance(?object), zero_or_more).
	:- info(leaf_instance/1, [
		comment is 'Returns, by backtracking, all class leaf instances.',
		argnames is ['Leaf']
	]).

	:- public(leaf_instances/1).
	:- mode(leaf_instances(-list), one).
	:- info(leaf_instances/1, [
		comment is 'List of all class leaf instances.',
		argnames is ['Leaves']
	]).

	:- public(leaf_class/1).
	:- mode(leaf_class(?object), zero_or_more).
	:- info(leaf_class/1, [
		comment is 'Returns, by backtracking, all class leaf subclasses.',
		argnames is ['Leaf']
	]).

	:- public(leaf_classes/1).
	:- mode(leaf_classes(-list), one).
	:- info(leaf_classes/1, [
		comment is 'List of all class leaf leaf subclasses.',
		argnames is ['Leaves']
	]).

	:- public(descendant_instance/1).
	:- mode(descendant_instance(?object), zero_or_more).
	:- info(descendant_instance/1, [
		comment is 'Returns, by backtracking, all class descendant instances.',
		argnames is ['Descendant']
	]).

	:- public(descendant_instances/1).
	:- mode(descendant_instances(-list), one).
	:- info(descendant_instances/1, [
		comment is 'List of all class descendant instances.',
		argnames is ['Descendants']
	]).

	:- public(descendant_class/1).
	:- mode(descendant_class(?object), zero_or_more).
	:- info(descendant_class/1, [
		comment is 'Returns, by backtracking, all class descendant subclasses.',
		argnames is ['Descendant']
	]).

	:- public(descendant_classes/1).
	:- mode(descendant_classes(-list), one).
	:- info(descendant_classes/1, [
		comment is 'List of all class descendant subclasses.',
		argnames is ['Descendants']
	]).

:- end_protocol.
