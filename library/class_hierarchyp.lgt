%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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
