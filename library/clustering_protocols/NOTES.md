
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


`clustering_protocols`
======================

This library provides protocols used in the implementation of machine
learning clustering algorithms. Datasets are represented as objects
implementing the `clustering_dataset_protocol` protocol. Clusterers are
represented as objects implementing the `clusterer_protocol` protocol.

This library also provides test datasets and a small smoke-test suite.
The shared test suite also includes cross-library comparison tests for
clusterers that implement the same protocols, allowing common datasets
and validation failures to be checked in one place.

Concrete clustering algorithms are intentionally out of scope for this
package. The goal is to provide a portable foundation for future
libraries such as `kmeans`.


API documentation
-----------------

Open the [../../apis/library_index.html#clustering_protocols](../../apis/library_index.html#clustering_protocols)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(clustering_protocols(loader)).


Testing
-------

To run the library smoke tests and shared comparison tests, load the
`tester.lgt` file:

	| ?- logtalk_load(clustering_protocols(tester)).


Test datasets
-------------

Several sample datasets are included in the `test_datasets` directory:

- `all_noise.lgt` — A synthetic 2D continuous dataset with 4
  examples and 2 continuous attributes (`x`, `y`). The examples are
  well separated, making the dataset useful for all-noise tests where a
  density-based clusterer should reject every point under a small
  epsilon radius.

- `bridge_noise.lgt` — A synthetic 2D continuous dataset with 10
  examples and 2 continuous attributes (`x`, `y`). The examples form
  two dense blobs connected by two sparse bridge points, making the
  dataset suitable for testing density-based algorithms that should keep
  the blobs separate while treating the bridge as noise.

- `dead_component_blobs.lgt` — A synthetic 2D continuous dataset with 6
  examples and 2 continuous attributes (`x`, `y`). The examples form
  two tiny ordered blobs sized so an over-specified Gaussian mixture
  with `first_k` initialization can drive one component fully dead,
  making the dataset useful for dead-component policy regression tests.

- `duplicate_points.lgt` — A synthetic 2D continuous dataset with 6
  examples and 2 continuous attributes (`x`, `y`). The examples include
  repeated coordinates forming a dense local cluster plus one isolated
  outlier, making the dataset useful for duplicate-point and
  density-threshold tests.

- `imbalanced_three_modes.lgt` — A synthetic 2D continuous dataset with
  9 examples and 2 continuous attributes (`x`, `y`). The examples form
  one dense blob plus two much sparser distant modes, making the dataset
  useful for imbalanced-cluster and Gaussian mixture stress tests.

- `iris_unlabeled.lgt` — A compact Iris-derived dataset with 9
  examples and 4 continuous attributes (`sepal_length`,
  `sepal_width`, `petal_length`, `petal_width`). It is derived from the
  classic Iris dataset but intentionally omits species labels so it can
  be used with unsupervised algorithms.

- `large_two_blobs.lgt` — A synthetic 2D continuous dataset with 100
  examples and 2 continuous attributes (`x`, `y`). The examples form
  two dense 5x10 grids that are well separated, making the dataset
  useful for performance benchmarks that need a larger deterministic
  density-based clustering workload than the small smoke-test datasets.

- `mixed_profiles.lgt` — A mixed-feature dataset with 6 examples, 2
  continuous attributes (`age`, `income`), and 2 discrete attributes
  (`channel`, `region`). This dataset is intended for clustering
  algorithms that support both numeric and categorical features.

- `scaling_bands.lgt` — A synthetic 2D continuous dataset with 6
  examples and 2 continuous attributes (`x`, `y`). The examples form
  two horizontal bands with large variation along `x`, making the
  dataset useful for tests that compare clustering behavior with feature
  scaling turned on versus off.

- `shopping_profiles.lgt` — A categorical dataset with 6 examples
  and 4 discrete attributes (`channel`, `region`, `loyalty`,
  `device`). The examples form two clear shopping-profile segments
  suitable for categorical clustering smoke tests.

- `single_blob.lgt` — A synthetic 2D continuous dataset with 6
  examples and 2 continuous attributes (`x`, `y`). The examples form a
  single compact blob suitable for one-cluster smoke tests.

- `two_blobs.lgt` — A synthetic 2D continuous dataset with 8
  examples and 2 continuous attributes (`x`, `y`). The examples form
  two compact, well-separated blobs suitable for deterministic
  clustering smoke tests.
