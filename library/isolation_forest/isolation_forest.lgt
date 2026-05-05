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
	imports(anomaly_detector_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Extended Isolation Forest (EIF) algorithm for anomaly detection. Implements the improved version described by Hariri et al. (2019) that uses random hyperplane cuts instead of axis-aligned cuts, eliminating score bias artifacts. Builds an ensemble of isolation trees from baseline training examples selected from a dataset object implementing the ``anomaly_dataset_protocol`` protocol. Missing attribute values are represented using anonymous variables.',
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, knn_distance, lof]
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

	% learn/3 - learn with specified options
	learn(Dataset, Model, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		% Get attribute information
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		length(AttributeNames, NumDimensions),
		^^baseline_training_examples(Dataset, Examples, Options),
		% Get all examples as numeric vectors; missing values are replaced
		% with random values drawn from the attribute range after ranges
		% are computed from the known values
		findall(AVs, member(_Id-_Class-AVs, Examples), AllAVs),
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
		build_diagnostics(Trees, Psi, AttributeNames, Options, Diagnostics),
		% Create model term (includes Ranges for training-time missing-value imputation)
		Model = if_model(Trees, Psi, AttributeNames, Attributes, Ranges, Diagnostics).

	check_anomaly_detector(Detector) :-
		(   Detector = if_model(Trees, Psi, AttributeNames, Attributes, Ranges, Diagnostics),
			valid_attribute_names(AttributeNames),
			valid_attribute_declarations(Attributes, AttributeNames),
			integer(Psi),
			Psi > 0,
			valid_ranges(Ranges, AttributeNames),
			valid_forest(Trees, AttributeNames),
			valid_detector_diagnostics(Trees, Psi, AttributeNames, Diagnostics) ->
			true
		;   domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(if_model(_Trees, _Psi, _AttributeNames, _Attributes, _Ranges, Diagnostics), Diagnostics).

	build_diagnostics(Trees, Psi, AttributeNames, Options, Diagnostics) :-
		length(Trees, TreeCount),
		length(AttributeNames, FeatureCount),
		Diagnostics = [
			model(isolation_forest),
			tree_count(TreeCount),
			subsample_size(Psi),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			options(Options)
		].

	% score/3 - compute anomaly score for an instance
	% Missing values are handled by sending the instance down both
	% branches for uncertain splits and computing a weighted average
	% path length using the subtree sizes as weights
	score(Model, Instance, Score) :-
		detector_data(Model, Trees, Psi, AttributeNames, Attributes, _Ranges, _Options),
		to_numeric_vector_with_missing(AttributeNames, Instance, Attributes, Vector, MissingMask),
		% Compute average path length across all trees
		compute_average_path_length(Trees, Vector, MissingMask, AvgPathLength),
		% Compute anomaly score: s(x) = 2^(-E(h(x)) / c(psi))
		average_path_length_bst(Psi, CPsi),
		(	CPsi > 0.0 ->
			Score is 2.0 ** (-(AvgPathLength / CPsi))
		;	Score = 0.5
		).

	% score_all/3 - compute scores for all instances
	score_all(Dataset, Model, SortedScores) :-
		detector_data(Model, _Trees, _Psi, AttributeNames, Attributes, _Ranges, _Options),
		findall(
			Score-Id-Class,
			(	Dataset::example(Id, Class, AVs),
				to_numeric_vector_with_missing(AttributeNames, AVs, Attributes, Vector, MissingMask),
				score_vector(Model, Vector, MissingMask, Score)
			),
			UnsortedPairs
		),
		msort(UnsortedPairs, SortedPairsAsc),
		reverse(SortedPairsAsc, SortedPairsDesc),
		^^extract_scores(SortedPairsDesc, SortedScores).

	score_vector(if_model(Trees, Psi, _, _, _, _), Vector, MissingMask, Score) :-
		compute_average_path_length(Trees, Vector, MissingMask, AvgPathLength),
		average_path_length_bst(Psi, CPsi),
		(	CPsi > 0.0 ->
			Score is 2.0 ** (-(AvgPathLength / CPsi))
		;	Score = 0.5
		).

	% export_to_clauses/4 - exports detector as a clause
	export_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Model) :-
		detector_data(Model, Trees, Psi, AttributeNames, _Attributes, _Ranges, Diagnostics),
		length(Trees, NumTrees),
		length(AttributeNames, NumDimensions),
		memberchk(options(Options), Diagnostics),
		format('Extended Isolation Forest Model~n', []),
		format('==============================~n~n', []),
		format('Number of trees:    ~w~n', [NumTrees]),
		format('Subsample size:     ~w~n', [Psi]),
		format('Number of features: ~w~n', [NumDimensions]),
		format('Features:           ~w~n', [AttributeNames]),
		^^print_anomaly_detector_template(Model),
		format('Options:            ~w~n~n', [Options]),
		format('Trees:~n', []),
		print_trees(Trees, 1).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(if_model(_Trees, _Psi, _AttributeNames, _Attributes, _Ranges, _Diagnostics), if_model('Trees', 'Psi', 'AttributeNames', 'Attributes', 'Ranges', 'Diagnostics')).

	detector_data(Detector, Trees, Psi, AttributeNames, Attributes, Ranges, Diagnostics) :-
		Detector =.. [_Functor, Trees, Psi, AttributeNames, Attributes, Ranges, Diagnostics].

	valid_attribute_names(AttributeNames) :-
		valid(list(atom), AttributeNames),
		AttributeNames \== [].

	valid_attribute_declarations(Attributes, AttributeNames) :-
		valid(list(pair), Attributes),
		length(Attributes, Count),
		length(AttributeNames, Count),
		valid_attribute_declarations_(Attributes, AttributeNames).

	valid_attribute_declarations_([], []).
	valid_attribute_declarations_([Attribute-Values| Attributes], [AttributeName| AttributeNames]) :-
		Attribute == AttributeName,
		atom(Attribute),
		(   Values == continuous ->
			true
		;   valid(list(nonvar), Values),
			Values \== []
		),
		valid_attribute_declarations_(Attributes, AttributeNames).

	valid_ranges(Ranges, AttributeNames) :-
		valid(list(compound), Ranges),
		length(Ranges, Count),
		length(AttributeNames, Count),
		valid_named_ranges_(Ranges, AttributeNames).

	valid_named_ranges_([], []).
	valid_named_ranges_([Attribute-Minimum-Maximum| Ranges], [AttributeName| AttributeNames]) :-
		Attribute == AttributeName,
		atom(Attribute),
		number(Minimum),
		number(Maximum),
		Minimum =< Maximum,
		valid_named_ranges_(Ranges, AttributeNames).

	valid_ranges_([]).
	valid_ranges_([Minimum-Maximum| Ranges]) :-
		number(Minimum),
		number(Maximum),
		Minimum =< Maximum,
		valid_ranges_(Ranges).

	valid_forest(Trees, AttributeNames) :-
		valid(list(compound), Trees),
		Trees \== [],
		length(AttributeNames, FeatureCount),
		valid_forest_(Trees, FeatureCount).

	valid_forest_([], _).
	valid_forest_([Tree| Trees], FeatureCount) :-
		valid_tree(Tree, FeatureCount),
		valid_forest_(Trees, FeatureCount).

	valid_tree(external(Size), _FeatureCount) :-
		integer(Size),
		Size > 0.
	valid_tree(internal(Normal, Intercept, NodeRanges, Left, Right), FeatureCount) :-
		valid_vector(Normal, FeatureCount),
		valid_vector(Intercept, FeatureCount),
		valid_ranges_by_count(NodeRanges, FeatureCount),
		valid_tree(Left, FeatureCount),
		valid_tree(Right, FeatureCount).

	valid_vector(Vector, FeatureCount) :-
		valid(list(number), Vector),
		length(Vector, FeatureCount).

	valid_ranges_by_count(Ranges, FeatureCount) :-
		valid(list(pair), Ranges),
		length(Ranges, FeatureCount),
		valid_ranges_(Ranges).

	valid_detector_options(Options) :-
		valid(list(compound), Options),
		catch(^^check_options(Options), _Error, fail).

	valid_detector_diagnostics(Trees, Psi, AttributeNames, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(isolation_forest), Diagnostics),
		length(Trees, TreeCount),
		memberchk(tree_count(TreeCount), Diagnostics),
		memberchk(subsample_size(Psi), Diagnostics),
		memberchk(attribute_names(AttributeNames), Diagnostics),
		length(AttributeNames, FeatureCount),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		valid_detector_options(Options).

	print_trees([], _).
	print_trees([Tree| Trees], N) :-
		tree_depth(Tree, Depth),
		tree_size(Tree, Size),
		format('  Tree ~w: depth=~w, nodes=~w~n', [N, Depth, Size]),
		N1 is N + 1,
		print_trees(Trees, N1).

	tree_depth(external(_), 0).
	tree_depth(internal(_, _, _, Left, Right), Depth) :-
		tree_depth(Left, LeftDepth),
		tree_depth(Right, RightDepth),
		Depth is max(LeftDepth, RightDepth) + 1.

	tree_size(external(_), 1).
	tree_size(internal(_, _, _, Left, Right), Size) :-
		tree_size(Left, LeftSize),
		tree_size(Right, RightSize),
		Size is LeftSize + RightSize + 1.

	% ===================================================================
	% Internal predicates
	% ===================================================================

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
		compute_ranges(Data, NumDimensions, NodeRanges),
		% Generate random normal vector (hyperplane direction)
		generate_normal_vector(NumDimensions, ExtLevel, Normal),
		% Generate random intercept point within the data range
		random_point_in_ranges(NodeRanges, Intercept),
		% Split data using the hyperplane: (x - p) . n <= 0 goes left
		split_by_hyperplane(Data, Normal, Intercept, Left, Right),
		% If split is degenerate (all to one side), make it a leaf
		(	(Left == [] ; Right == []) ->
			Tree = external(Size)
		;	NextDepth is CurrentDepth + 1,
			build_itree(Left, MaxDepth, NextDepth, NumDimensions, ExtLevel, LeftTree),
			build_itree(Right, MaxDepth, NextDepth, NumDimensions, ExtLevel, RightTree),
			Tree = internal(Normal, Intercept, NodeRanges, LeftTree, RightTree)
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

	% compute_average_path_length/4 - compute average path length across all trees
	% handling missing values
	compute_average_path_length(Trees, Vector, MissingMask, AvgPathLength) :-
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
	path_length(internal(Normal, Intercept, _Ranges, Left, Right), Vector, CurrentLength, PathLength) :-
		dot_shifted(Vector, Intercept, Normal, DotProduct),
		NextLength is CurrentLength + 1.0,
		(	DotProduct =< 0.0 ->
			path_length(Left, Vector, NextLength, PathLength)
		;	path_length(Right, Vector, NextLength, PathLength)
		).

	% path_length_missing/5 - compute path length with missing value handling
	% When a split depends on one or more missing dimensions, score both
	% branches and weight them by the number of training instances routed
	% to each subtree when the tree was built.
	path_length_missing(external(Size), _, _, CurrentLength, PathLength) :-
		average_path_length_bst(Size, Adjustment),
		PathLength is CurrentLength + Adjustment.
	path_length_missing(internal(Normal, Intercept, Ranges, Left, Right), Vector, MissingMask, CurrentLength, PathLength) :-
		NextLength is CurrentLength + 1.0,
		(	split_decision_with_missing(Vector, Intercept, Normal, MissingMask, Ranges, branch) ->
			path_length_missing(Left, Vector, MissingMask, NextLength, LeftPathLength),
			path_length_missing(Right, Vector, MissingMask, NextLength, RightPathLength),
			subtree_size(Left, LeftSize),
			subtree_size(Right, RightSize),
			weighted_path_length(LeftPathLength, LeftSize, RightPathLength, RightSize, PathLength)
		;	split_decision_with_missing(Vector, Intercept, Normal, MissingMask, Ranges, left) ->
			path_length_missing(Left, Vector, MissingMask, NextLength, PathLength)
		;	path_length_missing(Right, Vector, MissingMask, NextLength, PathLength)
		).

	split_decision_with_missing(Vector, Intercept, Normal, MissingMask, Ranges, Decision) :-
		dot_shifted_interval(Vector, Intercept, Normal, MissingMask, Ranges, MinDotProduct, MaxDotProduct),
		(	MaxDotProduct =< 0.0 ->
			Decision = left
		;	MinDotProduct > 0.0 ->
			Decision = right
		;	Decision = branch
		).

	dot_shifted_interval(Vector, Intercept, Normal, MissingMask, Ranges, MinDotProduct, MaxDotProduct) :-
		dot_shifted_interval(Vector, Intercept, Normal, MissingMask, Ranges, 0.0, MinDotProduct, 0.0, MaxDotProduct).

	dot_shifted_interval([], [], [], [], [], MinDotProduct, MinDotProduct, MaxDotProduct, MaxDotProduct).
	dot_shifted_interval([Xi| Xs], [Pi| Ps], [Ni| Ns], [false| Ms], [_-_| Rs], Min0, MinDotProduct, Max0, MaxDotProduct) :-
		Contribution is (Xi - Pi) * Ni,
		Min1 is Min0 + Contribution,
		Max1 is Max0 + Contribution,
		dot_shifted_interval(Xs, Ps, Ns, Ms, Rs, Min1, MinDotProduct, Max1, MaxDotProduct).
	dot_shifted_interval([_| Xs], [Pi| Ps], [Ni| Ns], [true| Ms], [Min-Max| Rs], Min0, MinDotProduct, Max0, MaxDotProduct) :-
		minimum_and_maximum_contribution(Min, Max, Pi, Ni, MinimumContribution, MaximumContribution),
		Min1 is Min0 + MinimumContribution,
		Max1 is Max0 + MaximumContribution,
		dot_shifted_interval(Xs, Ps, Ns, Ms, Rs, Min1, MinDotProduct, Max1, MaxDotProduct).

	minimum_and_maximum_contribution(MinimumValue, MaximumValue, InterceptValue, Coefficient, MinimumContribution, MaximumContribution) :-
		Contribution1 is (MinimumValue - InterceptValue) * Coefficient,
		Contribution2 is (MaximumValue - InterceptValue) * Coefficient,
		(	Contribution1 =< Contribution2 ->
			MinimumContribution = Contribution1,
			MaximumContribution = Contribution2
		;	MinimumContribution = Contribution2,
			MaximumContribution = Contribution1
		).

	weighted_path_length(LeftPathLength, LeftSize, RightPathLength, RightSize, PathLength) :-
		TotalSize is LeftSize + RightSize,
		(	TotalSize > 0 ->
			PathLength is (LeftPathLength * LeftSize + RightPathLength * RightSize) / TotalSize
		;	PathLength is (LeftPathLength + RightPathLength) / 2.0
		).

	subtree_size(external(Size), Size).
	subtree_size(internal(_, _, _, Left, Right), Size) :-
		subtree_size(Left, LeftSize),
		subtree_size(Right, RightSize),
		Size is LeftSize + RightSize.

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
	default_option(baseline_class_values([normal])).
	default_option(baseline_selection_policy(reject)).

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
	valid_option(baseline_class_values(BaselineClassValues)) :-
		^^valid_baseline_class_values(BaselineClassValues).
	valid_option(baseline_selection_policy(Policy)) :-
		valid(one_of(atom, [reject, filter]), Policy).

:- end_object.
