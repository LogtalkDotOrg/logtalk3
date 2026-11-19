.. _library_emeans_clusterer:

``emeans_clusterer``
====================

e-Means clusterer for continuous attributes. The default implementation
follows `Algorithm 1 from Bessa et
al. <https://arxiv.org/html/2611.09501v1#S3>`__, an EMA mini-batch
alternative to Lloyd's k-means. Use the ``algorithm(ema_lloyd)`` option
for EMA training followed by bounded Lloyd refinement.

The library implements the ``clusterer_protocol`` defined in the
``clustering_protocols`` library. It provides predicates for learning a
clusterer from a dataset, assigning new instances to clusters, and
exporting the learned clusterer as a list of predicate clauses or to a
file.

Datasets are represented as objects implementing the
``clustering_dataset_protocol`` protocol from the
``clustering_protocols`` library.

API documentation
-----------------

Open the
`../../apis/library_index.html#emeans-clusterer <../../apis/library_index.html#emeans-clusterer>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(emeans_clusterer(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(emeans_clusterer(tester)).

To run the performance benchmark suite, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(emeans_clusterer(tester_performance)).

The ``ema_lloyd`` scaled benchmark varies the number of rows and
clusters and reports timings for both the default refinement cap and a
one-update cap.

On a 5,120-row fixture, the one-update cap stops with changed
assignments, while the default cap verifies stability after two updates
(three assignment passes).

Usage
-----

Use the ``emeans_clusterer(Random)`` object to select a random generator
object and algorithm that provides the ``random/1``, ``randomize/1``,
``get_seed/1``, and ``set_seed/1`` predicates. For example:

::

   | ?- emeans_clusterer(fast_random(as183))::learn(Dataset, Model, [seed(42)]).

The convenience object ``emeans_clusterer`` uses
``fast_random(xoshiro128pp)``.

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **EMA Mini-batch Updates**: Centroids are maintained as the ratio of
  exponentially-decayed sums and counts; older assignments are forgotten
  at a controlled rate.
- **Automatic Hyper-parameters**: Batch size and peak step size are set
  from the published scaling laws (no extra knobs beyond ``k`` and
  ``epochs``).
- **Cosine Annealing**: The cosine schedule targets a minimum step size
  of 10 % of the peak.
- **Dead-cluster Revival**: The published mode splits count-weighted
  donor clusters every five batches when a cluster falls below 10% of
  mean EMA mass.
- **Finalization Stage**: The published mode computes means from the
  last epoch's assignments or returns a separate slow EMA, without a new
  assignment pass.
- **Initialization**: Random training rows by default; ``first_k`` and
  deterministic ``spread`` are also available.
- **Reproducible Mini-batches**: A user-supplied seed controls the
  Fisher-Yates shuffle used for mini-batch order. The shuffle uses the
  portable ``avltree`` dictionary without destructive updates.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Rich Training Diagnostics**: Learned clusterers report training
  example count, convergence status, epoch count, and final centroid
  shift.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``k(K)``: Number of clusters to learn. Default is ``2``.
- ``algorithm(Algorithm)``: ``published`` (default) or ``ema_lloyd``
  (EMA training followed by bounded Lloyd refinement).
- ``epochs(Epochs)``: Number of epochs, or the epoch cap in
  ``ema_lloyd`` mode. Default is ``5``.
- ``tolerance(Tolerance)``: ``ema_lloyd`` only. Maximum net centroid
  shift for early stopping, checked after the second epoch and skipped
  when revival reseeds a cluster. Default is ``1.0e-6``.
- ``initialization(Initialization)``: ``random`` (published default),
  ``spread`` (``ema_lloyd`` default), or ``first_k``.
- ``feature_scaling(FeatureScaling)``: ``off`` (published default) or
  ``on`` (``ema_lloyd`` default) for z-score standardization.
- ``finalization(Method)``: Published mode only. ``last_epoch_mean``
  (default) or ``slow_ema(HalfLife)``, with a positive half-life
  measured in batch updates.
- ``seed(Seed)``: An integer passed to the generator's ``randomize/1``
  or a state returned by its ``get_seed/1``. Default is ``1``. The
  clusterer checks only that the seed is ground and leaves its
  interpretation to the generator. Training initializes and advances the
  shared state; callers can save it with ``get_seed/1`` and restore it
  with ``set_seed/1`` if needed.
- ``refinement_iterations(N)``: ``ema_lloyd`` only. Positive cap on
  Lloyd centroid updates after training. Default is ``5``. An assignment
  check can follow the last update.

Diagnostics
-----------

The ``diagnostics/2`` predicate returns a list containing:

- ``model(emeans_clusterer)``
- ``centroid_count(Count)``
- ``training_example_count(Count)``
- ``convergence(Reason)`` (``tolerance`` or ``maximum_epochs``)
- ``epochs(Count)``
- ``epoch_shift(Shift)``: Maximum net centroid shift over the last
  training epoch, including revival.
- ``final_shift(Shift)``: In published mode, the maximum distance
  between an end-of-training centroid and its final value. In
  ``ema_lloyd`` mode, the maximum centroid movement in any individual
  refinement update.
- ``finalization(Method)``: Published mode only. ``last_epoch_mean`` or
  ``slow_ema``.
- ``last_epoch_occupied_cluster_count(Count)``: Last-epoch mean only.
  Number of clusters that received at least one assignment during the
  final training epoch. It is not a reassignment count for the returned
  model.
- ``refinement_status(Status)``: ``ema_lloyd`` only. ``stable`` when the
  returned model preserves the assignments used to compute its means and
  the last update did not reseed a cluster; otherwise
  ``iteration_limit``.
- ``refinement_updates(Count)``: ``ema_lloyd`` only. Number of completed
  refinement updates.
- ``occupied_cluster_count(Count)``: ``ema_lloyd`` only. Number of
  clusters assigned at least one training row by the returned model.
- ``options(Options)``

Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 4. For
example:

::

   emeans_clusterer(Encoders, Centroids, Options, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Centroids``: List of centroid vectors in cluster-id order.
- ``Options``: Effective training options used to learn the clusterer.
- ``Diagnostics``: Training diagnostics metadata returned by the
  ``diagnostics/2`` predicate.

Algorithm summary
-----------------

1. Sample ``k`` training rows without replacement unless another
   initialization is selected.
2. Initialise per-cluster sum/count with a tiny mass on the seeds.
3. Shuffle each epoch and process complete batches. Following the
   paper's floor-based update count, each epoch omits its shuffled
   remainder. The batch law is rounded to a power of two and capped at
   ``n``, with the paper's factor-of-eight reduction above two million
   rows.
4. Assign to the nearest centroids, accumulate sums and counts, and
   apply the EMA update with Equation 5's cosine schedule. Every fifth
   batch, sample live donors without replacement in proportion to their
   EMA counts, and split their mass with alternating coordinate
   perturbations (Appendix H).
5. Return the means of the assignments made during the last epoch. For
   clusters not assigned any row that epoch, retain the trained
   centroid. With slow-EMA finalization, return the separate EMA that
   tracked the same batch assignments and donor splits instead.

The ``ema_lloyd`` mode processes partial batches, checks convergence and
reseeds low-mass clusters at high-residual rows at epoch boundaries, and
finishes with bounded Lloyd refinement.

References
----------

1. Bessa et al. (2026) – "e-means: outperforming k-means (in one-tenth
   the epochs)". arXiv:2611.09501.
2. van den Oord et al. (2017) – VQ-VAE EMA centroid updates (original
   inspiration).
3. Lloyd (1982) – Least Squares Quantization in PCM.
4. MacQueen (1967) – Some Methods for Classification and Analysis of
   Multivariate Observations.

Limitations
-----------

- **Revival does not guarantee ``k`` occupied clusters.** The published
  donor perturbation leaves an all-zero centroid unchanged, and there
  may be fewer donors than dead clusters. In ``ema_lloyd`` mode, there
  may be no distinct row with a positive residual.
- **In ``ema_lloyd`` mode, convergence is local and refinement is
  bounded.** Epoch tolerance is about centroid movement, not
  assignments. Refinement reports assignment stability separately and
  may reach its cap first. A single scheduled mini-batch uses the peak
  learning rate; early stopping may end before the planned floor.
- **Mini-batches do not make training streaming or reduce the number of
  distance comparisons per epoch.** Training materializes the dataset
  and its encoded rows. Shared preprocessing validates and orders the
  raw bindings once for ordinary rows and, when scaling is enabled,
  collects numeric columns to fit the encoders. With ``n`` rows, ``k``
  clusters, and ``d`` features, nearest-centroid assignment costs
  ``O(n*k*d)`` per epoch, and the AVL shuffle adds ``O(n*log(n))`` time
  and allocation in the worst case despite reusing the initial index.
  Batch updates also traverse all cluster states.
- **Initialization, revival, and refinement in ``ema_lloyd`` mode can be
  substantial.** ``spread`` initialization caches nearest-centroid
  distances but still takes up to ``O(n*k*d)`` work. Each refinement
  update needs a full exact assignment pass, with indexed statistics
  accumulated in ``O(n*log(k))`` extra time for ``k > 1`` instead of
  rescanning assignments for each cluster. The final verification is
  another full assignment pass. An attempt to reseed clusters adds a
  residual pass and ranks candidates by residual.
- **The library does not reproduce the paper's GPU benchmarks.** It uses
  exact, in-memory CPU assignments rather than the optional approximate
  assignment path for very large cluster counts.
