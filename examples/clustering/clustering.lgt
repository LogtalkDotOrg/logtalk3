%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(clustering).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2018-09-04,
		comment is 'Clustering example using the Apache commons math Java library.'
	]).

	:- public(clusters/4).
	:- mode(clusters(+list(number), +positive_integer, +positive_integer, --list(list(number))), one).
	:- info(clusters/4, [
		comment is 'Performs clustering of a set of values.',
		argnames is ['Values', 'NumberOfClusters', 'MaxIterations', 'Clusters'],
		exceptions is [
			'Values is not instantiated' - instantiation_error,
			'Values neither a variable nor a list' - type_error(list(number), 'Values'),
			'An element Value of Values is not instantiated' - instantiation_error,
			'An element Value of Values is not a number' - type_error(number, 'Value'),
			'NumberOfClusters is not instantiated' - instantiation_error,
			'NumberOfClusters neither a variable nor a positive integer' - type_error(positive_integer, 'NumberOfClusters'),
			'MaxIterations is not instantiated' - instantiation_error,
			'MaxIterations neither a variable nor a positive integer' - type_error(positive_integer, 'MaxIterations'),
			'Clusters is not a variable' - type_error(var, 'Clusters'),
			'One of the arguments is illegal' - resource_error(math_illegal_argument_exception),
			'Clustering algorithm convergence failure' - resource_error(convergence_exception),
			'Number of values is too small' - resource_error(number_is_too_small_exception)
		]
	]).

	:- uses(list, [member/2]).
	:- uses(type, [check/3]).

	clusters(Values, NumberOfClusters, MaxIterations, Clusters) :-
		% type check all arguments to minimize the possible exceptions in
		% the Java side
		context(Context),
		check(list(number), Values, Context),
		check(positive_integer, NumberOfClusters, Context),
		check(positive_integer, MaxIterations, Context),
		check(var, Clusters, Context),
		% data must be passed to the clustering method using an instance of
		% a class that implements the Clusterable interface; we use here the
		% DoublePoint class that's also provided by the Apache Math library
		java('java.util.ArrayList')::new(List),
		forall(
			member(Value, Values),
			(	java(array(double))::new([Value], Array),
				java('org.apache.commons.math3.ml.clustering.DoublePoint')::new([Array], DoublePoint),
				java(List)::add(DoublePoint)
			)
		),
		java('org.apache.commons.math3.ml.clustering.KMeansPlusPlusClusterer')::new([NumberOfClusters, MaxIterations], KMeansPlusPlusClusterer),
		catch(
			java(KMeansPlusPlusClusterer, Results)::cluster(List),
			error(_, JavaException),
			convert_cluster_java_exception(JavaException, Context)
		),
		% we should get the same number of clusters that we specified
		% but just in case ...
		java(Results, Size)::size,
		Limit is Size - 1,
		% getting the set of computed clusters requires unwrapping the
		% wrapped DoublePoint instances
		findall(
			Cluster,
			(	between(0, Limit, N),
				java(Results, Result)::get(N),
				java(Result, Points)::getPoints,
				java(Points, Array)::toArray,
				java::array_list(Array, DoublePoints),
				findall(
					ClusterValue,
					(	member(DoublePoint, DoublePoints),
						java(DoublePoint, ArrayDouble)::getPoint,
						java::array_list(ArrayDouble, [ClusterValue])
					),
					Cluster
				)
			),
			Clusters
		).

	convert_cluster_java_exception('org.apache.commons.math3.exception.MathIllegalArgumentException', Context) :-
		throw(error(resource_error(math_illegal_argument_exception), Context)).
	convert_cluster_java_exception('org.apache.commons.math3.exception.ConvergenceException', Context) :-
		throw(error(resource_error(convergence_exception), Context)).
	convert_cluster_java_exception('org.apache.commons.math3.exception.NumberIsTooSmallException', Context) :-
		throw(error(resource_error(number_is_too_small_exception), Context)).

:- end_object.
