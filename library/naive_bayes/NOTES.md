________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`naive_bayes`
=============

Naive Bayes classifier supporting both categorical and continuous (Gaussian)
features with Laplace smoothing.

The library implements the `classifier_protocol` defined in the
`classifier_protocols` library. It provides predicates for learning a 
classifier from a dataset, using it to make predications, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classifier_protocols` library. See `test_files` directory
for examples.


API documentation
-----------------

Open the [../../docs/library_index.html#naive_bayes](../../docs/library_index.html#naive_bayes)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(naive_bayes(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(naive_bayes(tester)).


Features
--------

- **Categorical Features**: Handles discrete-valued features with Laplace smoothing
- **Continuous Features**: Uses Gaussian (normal) distribution for numeric features
- **Classifier Export**: Learned classifiers can be exported as predicate clauses
- **Probability Estimation**: Provides both class predictions and probability distributions


Usage
-----

### Learning a Classifier


    % Learn from a dataset object
    | ?- naive_bayes::learn(my_dataset, Classifier).
    ...

### Making Predictions

    % Predict class for a new instance and the probability distribution
    | ?- Instance = [...],
         naive_bayes::learn(my_dataset, Classifier),
         naive_bayes::predict(Classifier, Instance, PredictedClass),
         naive_bayes::predict_probability(Classifier, Instance, Probabilities).
    PredictedClass = ...,
    Probabilities = [...]
    ...

### Exporting the Classifier

Learned classifiers can be exported as a list of clauses or to a file for later use.

    % Export as predicate clauses
    | ?- naive_bayes::learn(my_dataset, Classifier),
         naive_bayes::classifier_to_clauses(Classifier, my_classifier, Clauses).
    Clauses = [my_classifier(...)]
    ...

    % Export to a file
    | ?- naive_bayes::learn(my_dataset, Classifier),
         naive_bayes::classifier_to_file(Classifier, my_classifier, 'classifier.pl').
    ...

### Using a learned classifier

Learned and saved classifiers can later be used for predictions without needing
to access the original training dataset.

    % Later, load the file and use the classifier
    | ?- consult('classifier.pl'),
         my_classifier(Classifier),
         Instance = [...],
         naive_bayes::predict(Classifier, Instance, Class).
    Class = ...
    ...


Classifier Representation
-------------------------

The learned classifier is represented as a compound term with the functor chosen
by the user when exporting the classifier and arity 5. The default functor is
`nb_classifier`:

    nb_classifier(Classes, ClassPriors, AttributeNames, FeatureTypes, FeatureParams)

Where:

- `Classes`: List of class labels
- `ClassPriors`: List of `Class-Prior` probability pairs
- `AttributeNames`: List of attribute names in order
- `FeatureTypes`: List of types (`categorical` or `continuous`)
- `FeatureParams`: List of learned parameters for each feature


References
----------

1. Rish, I. (2001). "An empirical study of the naive Bayes classifier".
2. Russell, S. & Norvig, P. (2020). "Artificial Intelligence: A Modern Approach".
3. Mitchell, T. (1997). "Machine Learning". Chapter 6: Bayesian Learning.
