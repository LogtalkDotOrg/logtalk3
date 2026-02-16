.. _library_classifier_protocols:

``classifier_protocols``
========================

This library provides protocols used in the implementation of machine
learning algorithms, aka classifiers. Datasets are represented as
objects implementing the ``dataset_protocol`` protocol. Classifiers are
represented as objects implementing the ``classifier_protocol``
protocol.

Logtalk currently provides ``c45``, ``knn``, and ``naive_bayes``
classifiers. See these libraries documentation for details.

API documentation
-----------------

Open the
`../../apis/library_index.html#classifier_protocols <../../apis/library_index.html#classifier_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(classifier_protocols(loader)).
