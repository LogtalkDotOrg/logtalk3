%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(isolation_forest,
	implements(classifier_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Extended Isolation Forest (EIF) algorithm for anomaly detection. Implements the improved version described by Hariri et al. (2019) that uses random hyperplane cuts instead of axis-aligned cuts, eliminating score bias artifacts. Builds an ensemble of isolation trees from a dataset object implementing the ``dataset_protocol`` protocol. Missing attribute values are represented using anonymous variables.',
		remarks is [
			'Algorithm' - 'The Extended Isolation Forest builds an ensemble of isolation trees (iTrees) by recursively partitioning the data using random hyperplanes. Anomalous points, being few and different, require fewer partitions (shorter path lengths) to be isolated.',
			'Extended vs Original' - 'The original Isolation Forest uses axis-aligned splits (random attribute + random value), which introduces bias in anomaly scores along coordinate axes. The extended version uses random hyperplane cuts with arbitrary slopes, producing more consistent and reliable anomaly scores.',
			'Extension level' - 'The extension level controls the dimensionality of the random hyperplane cuts. Level 0 corresponds to the original axis-aligned Isolation Forest. The default level is ``d - 1`` (fully extended) where ``d`` is the number of dimensions.',
			'Prediction' - 'The ``predict/3`` predicate returns ``anomaly`` if the anomaly score is above the threshold (default: 0.5) and ``normal`` otherwise. The ``score_all/3`` predicate returns a sorted list of all instances with their corresponding scores and class labels. Predictions use by default the learned model options but can override them using the ``anomaly_threshold/1`` option.',
			'Anomaly score' - 'The anomaly score ``s(x)`` is computed as ``s(x) = 2^(-E(h(x))/c(psi))`` where ``E(h(x))`` is the average path length across all trees, ``c(psi)`` is the average path length of unsuccessful searches in a BST, and ``psi`` is the subsample size. Scores close to 1 indicate anomalies; scores below 0.5 indicate normal points.',
			'Discrete attributes' - 'Discrete (categorical) attributes are mapped to numeric indices based on their position in the attribute value list declared by the dataset. This allows the algorithm to handle datasets with mixed attribute types.',
			'Missing values' - 'Missing attribute values are represented using anonymous variables. During tree construction, missing values are replaced with random values drawn from the observed range of the corresponding attribute. During scoring, instances with missing values are sent down both branches of the tree and the path length is computed as the weighted average of the two branches.',
			'Classifier representation' - 'The learned model is represented as an ``if_model(Trees, SubsampleSize, AttributeNames, Attributes, Ranges, Options)`` compound term.'
		],
		see_also is [dataset_protocol, c45, random_forest]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns an isolation forest model from the given dataset object using the specified options. Valid options are ``number_of_trees/1`` (default: ``100``), ``subsample_size/1`` (default: ``256`` or the number of instances if smaller), ``extension_level/1`` (default: ``d - 1`` where ``d`` is the number of dimensions), and ``anomaly_threshold/1`` (default: ``0.5``).',
		argnames is ['Dataset', 'Model', 'Options']
	]).

	:- public(predict/4).
	:- mode(predict(+compound, +list, -atom, +list(compound)), one).
	:- info(predict/4, [
		comment is 'Predicts whether an instance is an anomaly or normal using the learned model and the anomaly threshold with the given options. The instance is a list of ``Attribute-Value`` pairs where missing values are represented using anonymous variables. Returns ``anomaly`` if the anomaly score is above the threshold, ``normal`` otherwise.',
		argnames is ['Model', 'Instance', 'Prediction', 'Options']
	]).

	:- public(score/3).
	:- mode(score(+compound, +list, -float), one).
	:- info(score/3, [
		comment is 'Computes the anomaly score for a given instance using the learned model. The instance is a list of ``Attribute-Value`` pairs where missing values are represented using anonymous variables. The score is in the range ``[0.0, 1.0]``. Scores close to ``1.0`` indicate anomalies. Scores close to ``0.5`` or below indicate normal instances.',
		argnames is ['Model', 'Instance', 'Score']
	]).

	:- public(score_all/3).
	:- mode(score_all(+object_identifier, +compound, -list), one).
	:- info(score_all/3, [
		comment is 'Computes the anomaly scores for all instances in the dataset. Returns a list of ``Id-Class-Score`` triples sorted by descending anomaly score.',
		argnames is ['Dataset', 'Model', 'Scores']
	]).

	:- uses(fast_random(xoshiro128pp), [
		normal/3 as random_normal/3, random/3 as random_float/3, permutation/2 as random_permutation/2
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(integer, [
		sequence/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2, nth1/3, reverse/2, take/3, valid/1 as is_list/1
	]).

	:- uses(numberlist, [
		min/2, max/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	% learn/2 - learn with default options
	learn(Dataset, Model) :-
		learn(Dataset, Model, []).

	% learn/3 - learn with specified options
	learn(Dataset, Model, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		% Get attribute information
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		length(AttributeNames, NumDimensions),
		% Get all examples as numeric vectors; missing values are replaced
		% with random values drawn from the attribute range after ranges
		% are computed from the known values
		findall(
			AVs,
			Dataset::example(_, _, AVs),
			AllAVs
		),
		% Compute ranges from known values for imputation
		compute_attribute_ranges(AttributeNames, AllAVs, Attributes, Ranges),
		% Convert to numeric vectors with random imputation of missing values
		to_numeric_vectors(AllAVs, AttributeNames, Attributes, Ranges, Vectors),
		length(Vectors, NumInstances),
		% Determine subsample size
		(	^^option(subsample_size(Psi), Options) ->
			true
		;	(NumInstances < 256 -> Psi = NumInstances ; Psi = 256)
		),
		% Determine extension level
		(	^^option(extension_level(ExtLevel), Options) ->
			true
		;	(NumDimensions > 1 -> ExtLevel is NumDimensions - 1 ; ExtLevel = 0)
		),
		% Determine number of trees
		^^option(number_of_trees(NumTrees), Options),
		% Compute max depth
		max_depth(Psi, MaxDepth),
		% Build the forest
		build_forest(Vectors, NumTrees, Psi, MaxDepth, NumDimensions, ExtLevel, Trees),
		% Create model term (includes Ranges for scoring missing values)
		Model = if_model(Trees, Psi, AttributeNames, Attributes, Ranges, Options).

	% score/3 - compute anomaly score for an instance
	% Missing values are handled by sending the instance down both
	% branches and computing a weighted average path length
	score(Model, Instance, Score) :-
		Model =.. [_, Trees, Psi, AttributeNames, Attributes, Ranges, _Options],
		to_numeric_vector_with_missing(AttributeNames, Instance, Attributes, Vector, MissingMask),
		% Compute average path length across all trees
		compute_average_path_length(Trees, Vector, MissingMask, Ranges, AvgPathLength),
		% Compute anomaly score: s(x) = 2^(-E(h(x)) / c(psi))
		average_path_length_bst(Psi, CPsi),
		(	CPsi > 0.0 ->
			Score is 2.0 ** (-(AvgPathLength / CPsi))
		;	Score = 0.5
		).

	% predict/3 - predict anomaly or normal using default options
	predict(Model, Instance, Prediction) :-
		Model =.. [_, _, _, _, _, _, Options],
		predict(Model, Instance, Prediction, Options).

	% predict/4 - predict anomaly or normal using specified options
	predict(Model, Instance, Prediction, UserOptions) :-
		^^check_options(UserOptions),
		Model =.. [_, _, _, _, _, _, ModelOptions],
		(	member(anomaly_threshold(_), UserOptions) ->
			UpdateUserOptions = UserOptions
		;	memberchk(anomaly_threshold(ModelThreshold), ModelOptions),
			UpdateUserOptions = [anomaly_threshold(ModelThreshold)| UserOptions]
		),
		^^merge_options(UpdateUserOptions, Options),
		^^option(anomaly_threshold(Threshold), Options),
		score(Model, Instance, Score),
		(	Score >= Threshold ->
			Prediction = anomaly
		;	Prediction = normal
		).

	% score_all/3 - compute scores for all instances
	score_all(Dataset, Model, SortedScores) :-
		Model =.. [_, _, _, AttributeNames, Attributes, Ranges, _],
		findall(
			Score-Id-Class,
			(	Dataset::example(Id, Class, AVs),
				to_numeric_vector_with_missing(AttributeNames, AVs, Attributes, Vector, MissingMask),
				score_vector(Model, Vector, MissingMask, Ranges, Score)
			),
			UnsortedPairs
		),
		msort(UnsortedPairs, SortedPairsAsc),
		reverse(SortedPairsAsc, SortedPairsDesc),
		extract_scores(SortedPairsDesc, SortedScores).

	score_vector(if_model(Trees, Psi, _, _, _, _), Vector, MissingMask, Ranges, Score) :-
		compute_average_path_length(Trees, Vector, MissingMask, Ranges, AvgPathLength),
		average_path_length_bst(Psi, CPsi),
		(	CPsi > 0.0 ->
			Score is 2.0 ** (-(AvgPathLength / CPsi))
		;	Score = 0.5
		).

	extract_scores([], []).
	extract_scores([Score-Id-Class| Rest], [Id-Class-Score| Scores]) :-
		extract_scores(Rest, Scores).

	% classifier_to_clauses/4 - exports classifier as a clause
	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Classifier =.. [_, Trees, Psi, AttributeNames, Attributes, Ranges, Options],
		Clause =.. [Functor, Trees, Psi, AttributeNames, Attributes, Ranges, Options].

	% classifier_to_file/4 - exports classifier to a file
	classifier_to_file(Dataset, Classifier, Functor, File) :-
		classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Functor, Stream) :-
		format(Stream, '% ~q(Trees, Psi, AttributeNames, Attributes, Ranges, Options)~n', [Functor]).

	write_clauses([], _).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	print_classifier(Model) :-
		Model =.. [_, Trees, Psi, AttributeNames, _Attributes, _Ranges, Options],
		length(Trees, NumTrees),
		length(AttributeNames, NumDimensions),
		format('Extended Isolation Forest Model~n', []),
		format('==============================~n~n', []),
		format('Number of trees:    ~w~n', [NumTrees]),
		format('Subsample size:     ~w~n', [Psi]),
		format('Number of features: ~w~n', [NumDimensions]),
		format('Features:           ~w~n', [AttributeNames]),
		format('Options:            ~w~n~n', [Options]),
		format('Trees:~n', []),
		print_trees(Trees, 1).

	print_trees([], _).
	print_trees([Tree| Trees], N) :-
		tree_depth(Tree, Depth),
		tree_size(Tree, Size),
		format('  Tree ~w: depth=~w, nodes=~w~n', [N, Depth, Size]),
		N1 is N + 1,
		print_trees(Trees, N1).

	tree_depth(external(_), 0).
	tree_depth(internal(_, _, Left, Right), Depth) :-
		tree_depth(Left, LeftDepth),
		tree_depth(Right, RightDepth),
		Depth is max(LeftDepth, RightDepth) + 1.

	tree_size(external(_), 1).
	tree_size(internal(_, _, Left, Right), Size) :-
		tree_size(Left, LeftSize),
		tree_size(Right, RightSize),
		Size is LeftSize + RightSize + 1.

	% ===================================================================
	% Internal predicates
	% ===================================================================

	% dataset_attributes/2 - get attribute name-values pairs from dataset
	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	% to_numeric_vectors/5 - convert all attribute-value pair lists to numeric vectors
	% with random imputation of missing values for training
	to_numeric_vectors([], _, _, _, []).
	to_numeric_vectors([AVs| Rest], AttributeNames, Attributes, Ranges, [Vector| Vectors]) :-
		to_numeric_vector_impute(AVs, AttributeNames, Attributes, Ranges, Vector),
		to_numeric_vectors(Rest, AttributeNames, Attributes, Ranges, Vectors).

	% to_numeric_vector_impute/5 - convert attribute-value pairs to numeric vector
	% replacing missing values with random values from the attribute range
	to_numeric_vector_impute(AVs, AttributeNames, Attributes, Ranges, Vector) :-
		to_numeric_vector_impute_(AttributeNames, AVs, Attributes, Ranges, Vector).

	to_numeric_vector_impute_([], _, _, _, []).
	to_numeric_vector_impute_([Name| Names], AVs, Attributes, Ranges, [NumValue| Rest]) :-
		memberchk(Name-Value, AVs),
		memberchk(Name-AttrType, Attributes),
		(	var(Value) ->
			% Missing value: replace with random value from the attribute range
			memberchk(Name-Min-Max, Ranges),
			(	Max > Min ->
				random_float(Min, Max, NumValue)
			;	NumValue = Min
			)
		;	AttrType == continuous ->
			NumValue is float(Value)
		;	% Discrete: map to index position
			is_list(AttrType),
			once(nth1(Index, AttrType, Value)),
			NumValue is float(Index)
		),
		to_numeric_vector_impute_(Names, AVs, Attributes, Ranges, Rest).

	% to_numeric_vector_with_missing/5 - convert attribute-value pairs to numeric vector
	% and a missing value mask for scoring. Missing values get a placeholder (0.0)
	% and the mask indicates which dimensions are missing.
	to_numeric_vector_with_missing([], _, _, [], []).
	to_numeric_vector_with_missing([Name| Names], AVs, Attributes, [NumValue| Rest], [Missing| MaskRest]) :-
		memberchk(Name-Value, AVs),
		memberchk(Name-AttrType, Attributes),
		(	var(Value) ->
			% Missing value: mark as missing, use placeholder
			Missing = true,
			NumValue = 0.0
		;	AttrType == continuous ->
			Missing = false,
			NumValue is float(Value)
		;	% Discrete: map to index position
			is_list(AttrType),
			once(nth1(Index, AttrType, Value)),
			Missing = false,
			NumValue is float(Index)
		),
		to_numeric_vector_with_missing(Names, AVs, Attributes, Rest, MaskRest).

	% compute_attribute_ranges/4 - compute min/max ranges for each attribute
	% from known (non-missing) values across all examples
	compute_attribute_ranges([], _, _, []).
	compute_attribute_ranges([Name| Names], AllAVs, Attributes, [Name-Min-Max| Rest]) :-
		memberchk(Name-AttrType, Attributes),
		findall(
			NumVal,
			(	member(AVs, AllAVs),
				member(Name-Value, AVs),
				nonvar(Value),
				(	AttrType == continuous ->
					NumVal is float(Value)
				;	is_list(AttrType),
					once(nth1(Index, AttrType, Value)),
					NumVal is float(Index)
				)
			),
			KnownValues
		),
		(	KnownValues == [] ->
			Min = 0.0, Max = 0.0
		;	min(KnownValues, Min),
			max(KnownValues, Max)
		),
		compute_attribute_ranges(Names, AllAVs, Attributes, Rest).

	% max_depth/2 - compute max tree depth: ceil(log2(psi))
	max_depth(Psi, MaxDepth) :-
		(	Psi =< 1 ->
			MaxDepth = 0
		;	MaxDepth is ceiling(log(Psi) / log(2))
		).

	% build_forest/7 - build the specified number of isolation trees
	build_forest(Vectors, NumTrees, Psi, MaxDepth, NumDimensions, ExtLevel, Trees) :-
		length(Vectors, N),
		build_forest_(NumTrees, Vectors, N, Psi, MaxDepth, NumDimensions, ExtLevel, Trees).

	build_forest_(0, _, _, _, _, _, _, []) :-
		!.
	build_forest_(I, Vectors, N, Psi, MaxDepth, NumDimensions, ExtLevel, [Tree| Trees]) :-
		I > 0,
		% Subsample
		subsample(Vectors, N, Psi, Sample),
		% Build isolation tree
		build_itree(Sample, MaxDepth, 0, NumDimensions, ExtLevel, Tree),
		I1 is I - 1,
		build_forest_(I1, Vectors, N, Psi, MaxDepth, NumDimensions, ExtLevel, Trees).

	% subsample/4 - take a random subsample of size Psi from vectors
	subsample(Vectors, N, Psi, Sample) :-
		(	Psi >= N ->
			Sample = Vectors
		;	random_permutation(Vectors, Shuffled),
			take(Psi, Shuffled, Sample)
		).

	% build_itree/6 - recursively build an isolation tree
	% Extended Isolation Forest: uses random hyperplane cuts
	build_itree(Data, MaxDepth, CurrentDepth, _NumDimensions, _ExtLevel, external(Size)) :-
		length(Data, Size),
		(Size =< 1 ; CurrentDepth >= MaxDepth),
		!.
	build_itree(Data, MaxDepth, CurrentDepth, NumDimensions, ExtLevel, Tree) :-
		length(Data, Size),
		Size > 1,
		CurrentDepth < MaxDepth,
		% Generate random normal vector (hyperplane direction)
		generate_normal_vector(NumDimensions, ExtLevel, Normal),
		% Generate random intercept point within the data range
		generate_intercept(Data, NumDimensions, Intercept),
		% Split data using the hyperplane: (x - p) . n <= 0 goes left
		split_by_hyperplane(Data, Normal, Intercept, Left, Right),
		% If split is degenerate (all to one side), make it a leaf
		(	(Left == [] ; Right == []) ->
			Tree = external(Size)
		;	NextDepth is CurrentDepth + 1,
			build_itree(Left, MaxDepth, NextDepth, NumDimensions, ExtLevel, LeftTree),
			build_itree(Right, MaxDepth, NextDepth, NumDimensions, ExtLevel, RightTree),
			Tree = internal(Normal, Intercept, LeftTree, RightTree)
		).

	% generate_normal_vector/3 - generate a random normal vector for hyperplane
	% Extension level controls how many dimensions are active (non-zero)
	% Level 0: only one dimension (axis-aligned, like original IForest)
	% Level d-1: all dimensions (fully extended)
	generate_normal_vector(NumDimensions, ExtLevel, Normal) :-
		ActiveDimensions is min(ExtLevel + 1, NumDimensions),
		% Generate ActiveDimensions random components
		generate_random_components(ActiveDimensions, Components),
		% Create full vector with zeros for inactive dimensions
		(	ActiveDimensions =:= NumDimensions ->
			Normal = Components
		;	% Randomly choose which dimensions are active
			create_dimension_indices(NumDimensions, Indices),
			random_permutation(Indices, ShuffledIndices),
			take(ActiveDimensions, ShuffledIndices, ActiveIndices),
			msort(ActiveIndices, SortedActiveIndices),
			fill_normal_vector(NumDimensions, 1, SortedActiveIndices, Components, Normal)
		).

	generate_random_components(0, []) :-
		!.
	generate_random_components(N, [Component| Components]) :-
		N > 0,
		random_normal(0.0, 1.0, Component),
		N1 is N - 1,
		generate_random_components(N1, Components).

	create_dimension_indices(N, Indices) :-
		sequence(1, N, Indices).

	% fill_normal_vector/5 - create a vector of length N, placing components
	% at active dimension positions and 0.0 at inactive positions
	fill_normal_vector(N, I, _, _, []) :-
		I > N,
		!.
	fill_normal_vector(N, I, [I| ActiveRest], [C| CompRest], [C| Rest]) :-
		!,
		I1 is I + 1,
		fill_normal_vector(N, I1, ActiveRest, CompRest, Rest).
	fill_normal_vector(N, I, ActiveIndices, Components, [0.0| Rest]) :-
		I1 is I + 1,
		fill_normal_vector(N, I1, ActiveIndices, Components, Rest).

	% generate_intercept/3 - generate a random intercept point within data range
	generate_intercept(Data, NumDimensions, Intercept) :-
		compute_ranges(Data, NumDimensions, Ranges),
		random_point_in_ranges(Ranges, Intercept).

	% compute_ranges/3 - compute min/max for each dimension
	compute_ranges(Data, NumDimensions, Ranges) :-
		compute_ranges(1, NumDimensions, Data, Ranges).

	compute_ranges(I, N, _, []) :-
		I > N,
		!.
	compute_ranges(I, N, Data, [Min-Max| Ranges]) :-
		I =< N,
		extract_dimension(Data, I, Values),
		min(Values, Min),
		max(Values, Max),
		I1 is I + 1,
		compute_ranges(I1, N, Data, Ranges).

	extract_dimension([], _, []).
	extract_dimension([Vector| Vectors], I, [Value| Values]) :-
		nth1(I, Vector, Value),
		extract_dimension(Vectors, I, Values).

	random_point_in_ranges([], []).
	random_point_in_ranges([Min-Max| Ranges], [Point| Points]) :-
		(	Max > Min ->
			random_float(Min, Max, Point)
		;	Point = Min
		),
		random_point_in_ranges(Ranges, Points).

	% split_by_hyperplane/5 - split data using hyperplane: (x - p) . n <= 0 goes left
	split_by_hyperplane([], _, _, [], []).
	split_by_hyperplane([X| Xs], Normal, Intercept, Left, Right) :-
		dot_shifted(X, Intercept, Normal, DotProduct),
		(	DotProduct =< 0.0 ->
			Left = [X| LeftRest],
			Right = RightRest
		;	Left = LeftRest,
			Right = [X| RightRest]
		),
		split_by_hyperplane(Xs, Normal, Intercept, LeftRest, RightRest).

	% dot_shifted/4 - compute (x - p) . n
	dot_shifted(X, P, N, Dot) :-
		dot_shifted(X, P, N, 0.0, Dot).

	dot_shifted([], [], [], Dot, Dot).
	dot_shifted([Xi| Xs], [Pi| Ps], [Ni| Ns], Dot0, Dot) :-
		Dot1 is Dot0 + (Xi - Pi) * Ni,
		dot_shifted(Xs, Ps, Ns, Dot1, Dot).

	% compute_average_path_length/5 - compute average path length across all trees
	% handling missing values
	compute_average_path_length(Trees, Vector, MissingMask, _Ranges, AvgPathLength) :-
		(	has_missing(MissingMask) ->
			compute_path_lengths_missing(Trees, Vector, MissingMask, 0.0, TotalPathLength, 0, NumTrees)
		;	compute_path_lengths(Trees, Vector, 0.0, TotalPathLength, 0, NumTrees)
		),
		(	NumTrees > 0 ->
			AvgPathLength is TotalPathLength / NumTrees
		;	AvgPathLength = 0.0
		).

	% has_missing/1 - check if any dimension is missing
	has_missing([true| _]) :-
		!.
	has_missing([_| Rest]) :-
		has_missing(Rest).

	compute_path_lengths([], _, Total, Total, Count, Count).
	compute_path_lengths([Tree| Trees], Vector, Total0, Total, Count0, Count) :-
		path_length(Tree, Vector, 0.0, PathLength),
		Total1 is Total0 + PathLength,
		Count1 is Count0 + 1,
		compute_path_lengths(Trees, Vector, Total1, Total, Count1, Count).

	compute_path_lengths_missing([], _, _, Total, Total, Count, Count).
	compute_path_lengths_missing([Tree| Trees], Vector, MissingMask, Total0, Total, Count0, Count) :-
		path_length_missing(Tree, Vector, MissingMask, 0.0, PathLength),
		Total1 is Total0 + PathLength,
		Count1 is Count0 + 1,
		compute_path_lengths_missing(Trees, Vector, MissingMask, Total1, Total, Count1, Count).

	% path_length/4 - compute path length for a single tree (no missing values)
	path_length(external(Size), _, CurrentLength, PathLength) :-
		average_path_length_bst(Size, Adjustment),
		PathLength is CurrentLength + Adjustment.
	path_length(internal(Normal, Intercept, Left, Right), Vector, CurrentLength, PathLength) :-
		dot_shifted(Vector, Intercept, Normal, DotProduct),
		NextLength is CurrentLength + 1.0,
		(	DotProduct =< 0.0 ->
			path_length(Left, Vector, NextLength, PathLength)
		;	path_length(Right, Vector, NextLength, PathLength)
		).

	% path_length_missing/5 - compute path length with missing value handling
	% Uses only the known dimensions to compute the dot product for routing.
	% Missing dimensions contribute zero to the dot product, meaning the
	% routing decision is based entirely on the known attribute values.
	path_length_missing(external(Size), _, _, CurrentLength, PathLength) :-
		average_path_length_bst(Size, Adjustment),
		PathLength is CurrentLength + Adjustment.
	path_length_missing(internal(Normal, Intercept, Left, Right), Vector, MissingMask, CurrentLength, PathLength) :-
		dot_shifted_masked(Vector, Intercept, Normal, MissingMask, DotProduct),
		NextLength is CurrentLength + 1.0,
		(	DotProduct =< 0.0 ->
			path_length_missing(Left, Vector, MissingMask, NextLength, PathLength)
		;	path_length_missing(Right, Vector, MissingMask, NextLength, PathLength)
		).

	% dot_shifted_masked/5 - compute (x - p) . n skipping missing dimensions
	dot_shifted_masked(X, P, N, Mask, Dot) :-
		dot_shifted_masked(X, P, N, Mask, 0.0, Dot).

	dot_shifted_masked([], [], [], [], Dot, Dot).
	dot_shifted_masked([_| Xs], [_| Ps], [_| Ns], [true| Ms], Dot0, Dot) :-
		!,
		% Skip missing dimension (zero contribution)
		dot_shifted_masked(Xs, Ps, Ns, Ms, Dot0, Dot).
	dot_shifted_masked([Xi| Xs], [Pi| Ps], [Ni| Ns], [false| Ms], Dot0, Dot) :-
		Dot1 is Dot0 + (Xi - Pi) * Ni,
		dot_shifted_masked(Xs, Ps, Ns, Ms, Dot1, Dot).

	% average_path_length_bst/2 - average path length of unsuccessful search
	% in a Binary Search Tree (BST) with n nodes
	% c(n) = 2*H(n-1) - 2*(n-1)/n for n > 2
	% c(2) = 1
	% c(1) = 0
	% c(0) = 0
	% where H(i) = ln(i) + gamma (Euler-Mascheroni constant)
	average_path_length_bst(N, C) :-
		(	N > 2 ->
			N1 is N - 1,
			% Euler-Mascheroni constant
			Gamma = 0.5772156649,
			H is log(N1) + Gamma,
			C is 2.0 * H - 2.0 * N1 / N
		;	N =:= 2 ->
			C = 1.0
		;	C = 0.0
		).

	% Default options
	default_option(number_of_trees(100)).
	default_option(anomaly_threshold(0.5)).

	% Option validation
	valid_option(number_of_trees(N)) :-
		valid(positive_integer, N).
	valid_option(subsample_size(N)) :-
		valid(positive_integer, N).
	valid_option(extension_level(N)) :-
		integer(N),
		N >= 0.
	valid_option(anomaly_threshold(T)) :-
		float(T),
		T >= 0.0,
		T =< 1.0.

:- end_object.
