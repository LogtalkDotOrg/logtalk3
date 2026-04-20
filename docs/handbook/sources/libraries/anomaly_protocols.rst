.. _library_anomaly_protocols:

``anomaly_protocols``
=====================

This library provides protocols used in the implementation of machine
learning anomaly-detection algorithms. Datasets are represented as
objects implementing the ``anomaly_dataset_protocol`` protocol. Anomaly
detectors are represented as objects implementing the
``anomaly_detector_protocol`` protocol.

This library also provides a reusable shared category, anomaly benchmark
datasets, and a small family smoke-test suite.

Shared category
---------------

The library includes one reusable category intended to be imported by
anomaly detector algorithm implementations:

- ``anomaly_common`` — shared ``learn/2``, ``predict/3-4``, file export,
  and dataset helper predicates.

This category keeps threshold-based prediction and export behavior
separate from the algorithm-specific learning, scoring, clause export,
and pretty-printing code.

Export header format
--------------------

The shared exporter in the ``anomaly_common`` category writes a header
before the exported clauses in the following format:

::

   % exported anomaly detector predicate: Functor/Arity
   % training dataset: Dataset
   % options: Options
   % Functor(Detector)
   Functor(Detector)

The exported clauses serialize the learned detector term as a single
predicate argument so that loading the file gives a detector term that
can be passed directly to the ``predict/3-4`` and ``score_all/3``
predicates.

When exporting a serialized detector term, using a noun such as
``detector/1`` or ``model/1`` is recommended.

API documentation
-----------------

Open the
`../../apis/library_index.html#anomaly_protocols <../../apis/library_index.html#anomaly_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(anomaly_protocols(loader)).

Testing
-------

To run the library smoke tests, shared category tests, and dataset
checks, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(anomaly_protocols(tester)).

Test datasets
-------------

Several sample datasets are included in the ``test_datasets`` directory:

- ``gaussian_anomalies.lgt`` — A synthetic 2D anomaly detection dataset
  with 48 examples and 2 continuous attributes (x, y). Normal points are
  sampled from a standard normal distribution centered at the origin.
  Anomalous points are placed far from the cluster center. Inspired by
  the canonical test case used in the Extended Isolation Forest paper by
  Hariri et al. (2019).

- ``malformed_anomalies.lgt`` — A negative fixture with invalid class
  labels for testing family-level dataset validation.

- ``mixed_anomalies.lgt`` — A small mixed-feature anomaly dataset with
  16 examples, 2 continuous attributes (age, income), and 2 categorical
  attributes (student, credit_rating). Includes missing values and
  uncommon feature combinations to exercise anomaly-detection code on
  heterogeneous data.

- ``mixed_distance_behaviors.lgt`` — A compact mixed-feature anomaly
  fixture with 8 examples, 2 continuous attributes (size, weight), and 2
  categorical attributes (color, shape). Intended for smoke-testing
  continuous plus categorical distance behavior and basic mixed-data
  handling.

- ``sensor_anomalies.lgt`` — A synthetic industrial sensor anomaly
  dataset with 40 examples and 3 continuous attributes (temperature,
  pressure, vibration). Contains missing values (14 examples with
  missing values, represented using anonymous variables). Normal
  readings cluster around typical operating ranges. Anomalous readings
  show extreme values indicating equipment malfunction.

- ``shuttle_anomalies.lgt`` — A subset of the Statlog Shuttle dataset
  with 50 examples and 9 continuous attributes representing sensor
  readings from the NASA Space Shuttle. Class 1 (Rad Flow) is the
  majority class (normal), while all other classes are treated as
  anomalies. Originally from Catlett, J. (1991). Available from the UCI
  Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/148/statlog+shuttle

- ``water_potability.lgt`` — A water potability dataset with 48 examples
  and 9 continuous attributes (pH, hardness, solids, chloramines,
  sulfate, conductivity, organic carbon, trihalomethanes, turbidity).
  Normal instances represent potable water samples within acceptable
  ranges. Anomalous instances represent water samples with hazardous
  contamination levels. Based on the publicly available Water Quality
  dataset (Kadiwal, A., 2020, Kaggle).
