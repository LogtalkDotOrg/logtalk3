.. _library_fcube:

``fcube``
=========

This folder contains a Logtalk port of FCube: An Efficient Prover for
Intuitionistic Propositional Logic available from:

::

   https://www.vidal-rosset.net/fCube/

The port includes portability changes plus changes to use an ordered
representation for sets. Also some code formatting changes for Logtalk
coding guidelines.

The port tests are adapted from the examples available from the web page
above.

To load this port and for sample queries, please see the ``SCRIPT.txt``
file.

For more information about FCube, see the following paper:

::

   @InProceedings{10.1007/978-3-642-16242-8_21,
       author="Ferrari, Mauro and Fiorentini, Camillo and Fiorino, Guido",
       editor="Ferm{\"u}ller, Christian G. and Voronkov, Andrei",
       title="fCube: An Efficient Prover for Intuitionistic Propositional Logic",
       booktitle="Logic for Programming, Artificial Intelligence, and Reasoning",
       year="2010",
       publisher="Springer Berlin Heidelberg",
       address="Berlin, Heidelberg",
       pages="294--301",
       isbn="978-3-642-16242-8"
   }

For sample queries, please see the ``SCRIPT.txt`` file.

API documentation
-----------------

Open the
`../../docs/library_index.html#fcube <../../docs/library_index.html#fcube>`__
link in a web browser.

Loading
-------

To load all entities in this port, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(fcube(loader)).

Testing
-------

To test this port predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(fcube(tester)).

Known issues
------------

This port cannot currently be loaded using LVM due to a clash with a
built-in operator. Also, several tests fail when running on Tau Prolog,
likely due to a bug in this system.
