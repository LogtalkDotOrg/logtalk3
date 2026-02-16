:- initialization((
	logtalk_load(types(loader)),
	logtalk_load(format(loader)),
	logtalk_load(options(loader)),
	logtalk_load(random(loader)),
	logtalk_load(classifier_protocols(loader)),
	% Load local c45 (with fix) instead of library c45
	logtalk_load('../c45/c45', [optimize(on)]),
	logtalk_load(random_forest),
	logtalk_load(['test_files/iris']),
	run_test
)).

run_test :-
	format('~n=== Testing c45 fix on bootstrap dataset ===~n', []),
	% Create bootstrap dataset
	SelectedNames = [sepal_length, sepal_width],
	SelectedAttributes = [sepal_length-continuous, sepal_width-continuous],
	random_forest<<create_bootstrap_dataset(iris, SelectedNames, SelectedAttributes, BootstrapDataset),
	format('Created: ~w~n', [BootstrapDataset]),
	% Test c45::learn on bootstrap dataset
	format('~nCalling c45::learn on bootstrap dataset...~n', []),
	catch(c45::learn(BootstrapDataset, Tree), E, (format('Error: ~w~n', [E]), Tree = error)),
	format('Tree: ~w~n', [Tree]),
	abolish_object(BootstrapDataset),
	% Now test random_forest::learn
	format('~n=== Testing random_forest::learn ===~n', []),
	catch(
		random_forest::learn(iris, Classifier, [number_of_trees(3), maximum_features_per_tree(2)]),
		E2,
		(format('Error: ~w~n', [E2]), Classifier = error)
	),
	format('Classifier: ~w~n', [Classifier]).

