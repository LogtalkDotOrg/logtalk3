.. _library_frequent_pattern_mining_protocols:

``frequent_pattern_mining_protocols``
=====================================

This library provides support entities for frequent itemset mining
algorithms. Transactional datasets are represented as objects
implementing the ``transaction_dataset_protocol`` protocol. The generic
``pattern_miner_protocol`` protocol and the ``pattern_miner_common``
category used by concrete miners are loaded from the
``pattern_mining_protocols`` core library.

This library also provides reusable transaction smoke-test datasets and
a small smoke-test suite.

API documentation
-----------------

Open the
`../../apis/library_index.html#frequent_pattern_mining_protocols <../../apis/library_index.html#frequent_pattern_mining_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(frequent_pattern_mining_protocols(loader)).

Testing
-------

To run the library smoke tests, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(frequent_pattern_mining_protocols(tester)).

Test datasets
-------------

Several sample transaction datasets are included in the
``test_datasets`` directory:

- ``market_basket_basics.lgt``: A compact transaction dataset with 6
  transactions and 5 items intended for frequent-itemset smoke tests.

- ``layered_baskets.lgt``: A transaction dataset with overlapping
  co-occurrence layers intended for support-count and candidate-pruning
  tests.

- ``deep_intersection_baskets.lgt``: A compact transaction dataset with
  one frequent length-4 itemset and multiple overlapping length-3
  itemsets intended to stress deeper vertical tidset intersections.
