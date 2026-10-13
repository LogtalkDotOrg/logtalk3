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


`mlp_classifier`
================

Multi-layer perceptron classifier for continuous, categorical, and mixed
tabular datasets. The implementation provides zero or more configurable hidden
layers, ReLU, hyperbolic tangent, or sigmoid hidden activation, stable softmax
output, and backpropagation training with L2 regularization.

The library implements the probabilistic classifier protocol from the
`classification_protocols` library and reuses its shared linear encoders.


API documentation
-----------------

Open the [../../apis/library_index.html#mlp-classifier](../../apis/library_index.html#mlp-classifier)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(mlp_classifier(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(mlp_classifier(tester)).

The test suite covers malformed and incomplete models, bias-only datasets,
stable cross-entropy and convergence branches, activation boundaries, and
multiclass probability ordering and normalization. Central finite differences
check every weight and bias in small networks with zero, one, and two hidden
layers across all three activations. The perturbation is `1.0e-6`; comparisons
use relative tolerance `1.0e-5` and absolute tolerance `1.0e-7`. ReLU gradient
checks avoid its nondifferentiable point, which has a separate boundary test.

Two-step numerical references cover momentum, L2 regularization, bias updates,
tracked and untracked convergence policies, and integer and float zero options.
The XOR model is trained once per test run for 50 epochs and reused for
prediction, validation, export, and printing. The independent `learn/2` test
still exercises default training. Clause coverage and these explicit behavioral
checks are complementary; full clause coverage does not imply full branch or
numerical coverage.

The principal regression groups are:

| Contract | Tests |
| --- | --- |
| Model validation | `mlp_classifier_validation_fixtures`, `mlp_classifier_check_incomplete_fixture`, `mlp_classifier_duplicate_option_precedence` |
| Zero-feature learning | `mlp_classifier_bias_only_configurations`, `mlp_classifier_bias_only_update`, `mlp_classifier_empty_dataset` |
| Stable loss and stopping | `mlp_classifier_saturated_loss`, `mlp_classifier_loss_shift_weight_average`, `mlp_classifier_convergence_branches` |
| Gradients and activations | `mlp_classifier_finite_difference_gradients`, `mlp_classifier_activation_boundaries`, `mlp_classifier_softmax_shift` |
| Optimized evaluation | `mlp_classifier_prepared_rows`, `mlp_classifier_forward_only_equivalence`, `mlp_classifier_affine_reference`, `mlp_classifier_transpose_product_reference` |
| Optimizer policies | `mlp_classifier_optimizer_reference`, `mlp_classifier_training_option_equivalence`, `mlp_classifier_update_policy_equivalence`, `mlp_classifier_tiny_parameter_update` |


Options
-------

- `hidden_layers/1` - list of positive layer sizes; an empty list selects a
  linear softmax model (default: `[8]`)
- `activation/1` - `relu`, `tanh`, or `sigmoid` (default: `relu`)
- `class_weights/1` - list of `Class-Weight` pairs assigning positive loss
  weights to selected classes; omitted classes use `1.0` (default: `[]`)
- `convergence_criterion/1` - `parameter_update` to compare the largest
  absolute individual parameter update in an epoch, not its net displacement,
  or `loss` to compare successive mean weighted
  cross-entropy losses (default: `parameter_update`)
- `learning_rate/1` - positive base learning rate (default: `0.05`)
- `learning_schedule/1` - `constant` or `inverse_scaling(Power)` (default: `inverse_scaling(0.25)`)
- `maximum_iterations/1` - maximum number of epochs (default: `500`)
- `momentum/1` - momentum coefficient greater than or equal to zero and less
  than one (default: `0.0`)
- `tolerance/1` - convergence threshold for the selected criterion (default: `1.0e-6`)
- `l2_regularization/1` - non-negative L2 penalty (default: `0.0001`)
- `feature_scaling/1` - standardize continuous attributes (default: `true`)
- `random_seed/1` - positive integer initialization and shuffling seed (default: `42`)
- `shuffle/1` - shuffle training examples before each epoch (default: `true`)


Classifier representation
-------------------------

	mlp_classifier(Classes, Encoders, Activation, Layers, Diagnostics)

Each layer is represented by `layer(Weights, Biases)`, where `Weights` is a
row-major matrix and `Biases` is a vector. Hidden layers use the configured
activation and the output layer uses softmax probabilities.

The learned model uses a `mlp_classifier/5` term representation by default.
Users can choose any outer functor while preserving the five arguments and
their order. Validation, prediction, diagnostics, export, and printing support
these custom functors; the diagnostics `model(mlp_classifier)` entry identifies
the algorithm, not the outer functor.

The `check_classifier/1` predicate requires a ground five-argument compound
term with valid layer dimensions and diagnostics consistent with its classes,
encoders, activation, hidden layers, and effective training options. It raises
an instantiation error for insufficiently instantiated terms and a classifier
domain error for ground invalid terms. The `valid_classifier/1` predicate
fails for either case. Validation does not fill in missing model fields.
Repeated options remain valid, with the first occurrence determining the
effective value.

Datasets with no declared attributes produce bias-only models with empty
encoders and zero-width first-layer weight rows. Both linear softmax models
and models with hidden layers support this case. Training examples must still
be nonempty.

Loss convergence monitors mean weighted cross-entropy computed directly from
logits using shifted log-sum-exp. Very small target probabilities are not
clipped, so confidently incorrect predictions retain their loss differences.
The mean divides by the number of training examples, not the sum of class
weights. L2 regularization affects weight updates but is not included in this
convergence metric; biases are not regularized.


Performance
-----------

Training settings, row class indices, and effective class weights are prepared
once before the epoch loop. Weight gradients are computed within the update
loop rather than materialized as outer-product matrices. Backpropagation
computes transpose-vector products without storing complete transposes and
preserves the algebra library's column summation order. Affine evaluation
produces each dot product plus its bias without an intermediate product
vector. Prediction and loss evaluation do not construct the backpropagation
input trace.

Zero momentum avoids velocity matrices, and the zero-momentum update path
omits L2 arithmetic when regularization is zero. Loss convergence omits unused
parameter-update magnitude tracking. These internal paths retain online SGD,
the public model representation, class ordering, and the existing random
initialization and shuffling behavior for positive-width inputs.

The `mlp_classifier_benchmarks::run/0` predicate prints five CPU and wall time
samples per workload after a warm-up. From this library directory, load the
optimized implementation, the XOR dataset, the operating-system library, and
the benchmarks:

    | ?- logtalk_load(loader),
         logtalk_load(os(loader)),
         logtalk_load(['test_datasets/xor', benchmarks], [optimize(on)]),
         mlp_classifier_benchmarks::run.

Training workloads use four XOR examples, hidden layers `[16, 8]`, 200 epochs,
seed 42, constant learning rate, and zero convergence tolerance. They separate
positive momentum, zero momentum, zero momentum with zero regularization,
and loss convergence. Prediction evaluates the same instance 2000 times.
Compare medians and variability with identical workloads, independently of
debug and coverage instrumentation. Allocation reductions do not guarantee
the same runtime gains for every dataset or backend.


Limitations
-----------

- Designed primarily for small to medium dense tabular datasets.
- Training uses online gradient descent without mini-batches or adaptive
  learning-rate optimizers.
- Dropout, batch normalization, sample weighting, and validation-based early
  stopping are not supported.
- Deep networks may exhibit vanishing or exploding gradients.
- Reproducible initialization assumes that classifier training calls are
  not executed concurrently, as the random generator has shared state.


References
----------

1. Rumelhart, D. E., Hinton, G. E., and Williams, R. J. (1986). "Learning representations by back-propagating errors".
2. Glorot, X. and Bengio, Y. (2010). "Understanding the difficulty of training deep feedforward neural networks".
3. He, K., Zhang, X., Ren, S., and Sun, J. (2015). "Delving deep into rectifiers".
