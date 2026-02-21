.. _library_statistics:

``statistics``
==============

The entities in this library define some useful predicates for
descriptive statistics. Data is represented as a list of numbers
(integers or floats). Use the object ``sample`` if your data represents
a sample. Use the object ``population`` if your data represents a
population.

The ``variance/2``, ``standard_deviation/2``, ``skewness/2``,
``kurtosis/2``, ``covariance/3``, ``standard_error/2``,
``correlation/3``, and ``rank_correlation/3`` predicates use different
formulas depending on whether the data represents a sample (dividing by
*N-1*) or a population (dividing by *N*). All other predicates share the
same implementation.

API documentation
-----------------

Open the
`../../apis/library_index.html#statistics <../../apis/library_index.html#statistics>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(statistics(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(statistics(tester)).

API overview
------------

Aggregation
~~~~~~~~~~~

============= ===========================
Predicate     Description
============= ===========================
``product/2`` Product of all list numbers
``sum/2``     Sum of all list numbers
============= ===========================

Extremes and Range
~~~~~~~~~~~~~~~~~~

============= ==========================
Predicate     Description
============= ==========================
``min/2``     Minimum value
``max/2``     Maximum value
``min_max/3`` Minimum and maximum values
``range/2``   Range (max - min)
============= ==========================

Central Tendency
~~~~~~~~~~~~~~~~

+-----------------------+----------------------------------------------+
| Predicate             | Description                                  |
+=======================+==============================================+
| ``arithmetic_mean/2`` | Arithmetic mean                              |
+-----------------------+----------------------------------------------+
| ``geometric_mean/2``  | Geometric mean                               |
+-----------------------+----------------------------------------------+
| ``harmonic_mean/2``   | Harmonic mean                                |
+-----------------------+----------------------------------------------+
| ``weighted_mean/3``   | Weighted mean                                |
+-----------------------+----------------------------------------------+
| ``trimmed_mean/3``    | Trimmed mean (removing a fraction of extreme |
|                       | values)                                      |
+-----------------------+----------------------------------------------+
| ``median/2``          | Median                                       |
+-----------------------+----------------------------------------------+
| ``modes/2``           | Modes (in ascending order)                   |
+-----------------------+----------------------------------------------+

.. _measures-of-position--quantiles:

Measures of Position / Quantiles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+---------------------------+------------------------------------------+
| Predicate                 | Description                              |
+===========================+==========================================+
| ``fractile/3``            | Fractile (quantile given a fraction in   |
|                           | (0.0, 1.0))                              |
+---------------------------+------------------------------------------+
| ``percentile/3``          | Percentile (quantile given a value in    |
|                           | (0, 100))                                |
+---------------------------+------------------------------------------+
| ``quartiles/4``           | Quartiles (Q1, Q2, Q3)                   |
+---------------------------+------------------------------------------+
| ``interquartile_range/2`` | Interquartile range (Q3 - Q1)            |
+---------------------------+------------------------------------------+

Measures of Dispersion
~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------+----------------------------------+
| Predicate                         | Description                      |
+===================================+==================================+
| ``variance/2``                    | Variance (sample or population)  |
+-----------------------------------+----------------------------------+
| ``standard_deviation/2``          | Standard deviation (sample or    |
|                                   | population)                      |
+-----------------------------------+----------------------------------+
| ``mean_deviation/2``              | Mean absolute deviation          |
+-----------------------------------+----------------------------------+
| ``median_deviation/2``            | Median absolute deviation        |
+-----------------------------------+----------------------------------+
| ``average_deviation/3``           | Average absolute deviation from  |
|                                   | a given central tendency         |
+-----------------------------------+----------------------------------+
| ``coefficient_of_variation/2``    | Coefficient of variation         |
+-----------------------------------+----------------------------------+
| ``relative_standard_deviation/2`` | Relative standard deviation      |
|                                   | (percentage)                     |
+-----------------------------------+----------------------------------+
| ``sum_of_squares/2``              | Sum of squared deviations from   |
|                                   | the mean                         |
+-----------------------------------+----------------------------------+
| ``standard_error/2``              | Standard error of the mean       |
+-----------------------------------+----------------------------------+

Measures of Shape
~~~~~~~~~~~~~~~~~

==================== ======================================
Predicate            Description
==================== ======================================
``skewness/2``       Moment skewness (sample or population)
``kurtosis/2``       Excess kurtosis (sample or population)
``central_moment/3`` K-th central moment
==================== ======================================

Measures of Association
~~~~~~~~~~~~~~~~~~~~~~~

====================== =====================================
Predicate              Description
====================== =====================================
``covariance/3``       Covariance (sample or population)
``correlation/3``      Pearson correlation coefficient
``rank_correlation/3`` Spearman rank correlation coefficient
====================== =====================================

Error Metrics
~~~~~~~~~~~~~

============================= =========================================
Predicate                     Description
============================= =========================================
``mean_squared_error/3``      Mean squared error between two lists
``root_mean_squared_error/3`` Root mean squared error between two lists
============================= =========================================

Normalization
~~~~~~~~~~~~~

=========================== ===========================================
Predicate                   Description
=========================== ===========================================
``z_normalization/2``       Z-score normalization (mean ~= 0, std ~= 1)
``min_max_normalization/2`` Min-max normalization (rescale to [0, 1])
=========================== ===========================================

.. _frequency--counting:

Frequency / Counting
~~~~~~~~~~~~~~~~~~~~

============================ ==========================================
Predicate                    Description
============================ ==========================================
``frequency_distribution/2`` Frequency distribution (Value-Count pairs)
============================ ==========================================

Validation
~~~~~~~~~~

=========== ================================
Predicate   Description
=========== ================================
``valid/1`` Term is a closed list of numbers
=========== ================================
