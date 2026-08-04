.. _library_multivariate_distributions:

``multivariate_distributions``
==============================

This library provides multivariate normal and Student's t sampling and
density predicates, Mahalanobis distance predicates, and
additive-log-ratio logistic-normal sampling predicates. Batch predicates
return samples as lists of row vectors.

API documentation
-----------------

Open the
`../../apis/library_index.html#multivariate_distributions <../../apis/library_index.html#multivariate_distributions>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(multivariate_distributions(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(multivariate_distributions(tester)).

Scope
-----

The ``multivariate_distributions(Random)`` parametric object accepts a
random source implementing the ``sampling_protocol`` protocol. Common
choices include ``random(xoshiro128pp)``, ``fast_random(xoshiro128pp)``,
and ``backend_random``.

Covariance and scale matrices may be singular positive-semidefinite
matrices. The predicates use eigendecomposition to operate on their
affine support and do not add diagonal jitter. Density predicates return
``0.0`` outside singular support and log-density predicates return
``negative_infinity``. Mahalanobis distance predicates throw an error
for points outside support.

For the multivariate Student's t distribution, the matrix argument is a
scale matrix. When the degrees of freedom are greater than two, the
covariance is the scale matrix multiplied by
``DegreesOfFreedom / (DegreesOfFreedom - 2)``.

The logistic-normal predicates use the additive log-ratio
representation. A latent vector of length d maps to a simplex vector of
length d+1, with the last simplex component used as the fixed reference
component.

Examples
--------

Generate one multivariate normal sample using a portable pseudo-random
source:

::

   | ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal(
            [0.0, 0.0], [[1.0, 0.5], [0.5, 1.0]], Sample
        ).

Generate a batch of five samples, factorizing the covariance matrix only
once:

::

   | ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal_samples(
            5, [0.0, 0.0], [[1.0, 0.5], [0.5, 1.0]], Samples
        ).

Compute a density and a Mahalanobis distance (these predicates do not
consume random values):

::

   | ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal_density(
            [1.0, 0.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 1.0]], Density
        ).
   Density = 0.09653235263005393.

   | ?- multivariate_distributions(random(xoshiro128pp))::mahalanobis_distance(
            [2.0, 0.0], [0.0, 0.0], [[4.0, 0.0], [0.0, 1.0]], Distance
        ).
   Distance = 1.0.

For a singular covariance matrix, density is evaluated on its affine
support and is zero outside it:

::

   | ?- multivariate_distributions(random(xoshiro128pp))::multivariate_normal_density(
            [1.0, 1.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], Density
        ).
   Density = 0.0.

Generate a multivariate Student's t sample with five degrees of freedom:

::

   | ?- multivariate_distributions(random(xoshiro128pp))::multivariate_t(
            5.0, [0.0, 0.0], [[1.0, 0.0], [0.0, 1.0]], Sample
        ).

Generate a logistic-normal probability vector. A two-dimensional latent
normal distribution produces three simplex components:

::

   | ?- multivariate_distributions(random(xoshiro128pp))::logistic_normal(
            [0.0, 0.0], [[1.0, 0.0], [0.0, 1.0]], Probabilities
        ).
