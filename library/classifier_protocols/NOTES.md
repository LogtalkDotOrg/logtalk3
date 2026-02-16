
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


`classifier_protocols`
======================

This library provides protocols used in the implementation of machine
learning algorithms, aka classifiers. Datasets are represented as objects
implementing the `dataset_protocol` protocol. Classifiers are represented
as objects implementing the `classifier_protocol` protocol.

Logtalk currently provides `c45` and `naive_bayes` classifiers.
See these libraries documentation for details.


API documentation
-----------------

Open the [../../apis/library_index.html#classifier_protocols](../../apis/library_index.html#classifier_protocols)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(classifier_protocols(loader)).


Comparison between classifiers
------------------------------

| Aspect | Naive Bayes | C4.5 |
|--------|-------------|------|
| **Speed** | Very fast | Slower (tree construction) |
| **Interpretability** | Probability-based | Decision rules |
| **Continuous Features** | Native (Gaussian) | Threshold splits |
| **Feature Dependencies** | Ignored | Captured through splits |
| **Export Format** | Clauses | Clauses |
