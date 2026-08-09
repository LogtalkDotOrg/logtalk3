.. _library_mlp_classifier:

``mlp_classifier``
==================

Multi-layer perceptron classifier for continuous, categorical, and mixed
tabular datasets. The implementation provides zero or more configurable
hidden layers, ReLU, hyperbolic tangent, or sigmoid hidden activation,
stable softmax output, and backpropagation training with L2
regularization.

The library implements the probabilistic classifier protocol from the
``classification_protocols`` library and reuses its shared linear
encoders.

API documentation
-----------------

Open the
`../../apis/library_index.html#mlp-classifier <../../apis/library_index.html#mlp-classifier>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(mlp_classifier(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(mlp_classifier(tester)).

Options
-------

- ``hidden_layers/1`` - list of positive layer sizes; an empty list
  selects a linear softmax model (default: ``[8]``)
- ``activation/1`` - ``relu``, ``tanh``, or ``sigmoid`` (default:
  ``relu``)
- ``class_weights/1`` - list of ``Class-Weight`` pairs assigning
  positive loss weights to selected classes; omitted classes use ``1.0``
  (default: ``[]``)
- ``convergence_criterion/1`` - ``parameter_update`` to compare the
  largest parameter update or ``loss`` to compare successive mean
  weighted cross-entropy losses (default: ``parameter_update``)
- ``learning_rate/1`` - positive base learning rate (default: ``0.05``)
- ``learning_schedule/1`` - ``constant`` or ``inverse_scaling(Power)``
  (default: ``inverse_scaling(0.25)``)
- ``maximum_iterations/1`` - maximum number of epochs (default: ``500``)
- ``momentum/1`` - momentum coefficient greater than or equal to zero
  and less than one (default: ``0.0``)
- ``tolerance/1`` - convergence threshold for the selected criterion
  (default: ``1.0e-6``)
- ``l2_regularization/1`` - non-negative L2 penalty (default:
  ``0.0001``)
- ``feature_scaling/1`` - standardize continuous attributes (default:
  ``true``)
- ``random_seed/1`` - positive integer initialization and shuffling seed
  (default: ``42``)
- ``shuffle/1`` - shuffle training examples before each epoch (default:
  ``true``)

Classifier representation
-------------------------

::

   mlp_classifier(Classes, Encoders, Activation, Layers, Diagnostics)

Each layer is represented by ``layer(Weights, Biases)``, where
``Weights`` is a row-major matrix and ``Biases`` is a vector. Hidden
layers use the configured activation and the output layer uses softmax
probabilities.

Limitations
-----------

- Designed primarily for small to medium dense tabular datasets.
- Training uses online gradient descent without mini-batches or adaptive
  learning-rate optimizers.
- Dropout, batch normalization, sample weighting, and validation-based
  early stopping are not supported.
- Deep networks may exhibit vanishing or exploding gradients.
- Reproducible initialization assumes that classifier training calls are
  not executed concurrently, as the random generator has shared state.

References
----------

1. Rumelhart, D. E., Hinton, G. E., and Williams, R. J. (1986).
   "Learning representations by back-propagating errors".
2. Glorot, X. and Bengio, Y. (2010). "Understanding the difficulty of
   training deep feedforward neural networks".
3. He, K., Zhang, X., Ren, S., and Sun, J. (2015). "Delving deep into
   rectifiers".
