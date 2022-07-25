.. _library_xml_parser:

``xml_parser``
==============

This folder contains a Logtalk version of John Fletcher's Prolog XML
parser:

https://binding-time.co.uk/index.php/Parsing_XML_with_Prolog

For a detailed description of this XML parser, please see the comments
in the ``xml.lgt`` source file or convert the automatically generated
documentation to HTML or PDF. For sample queries, please see the
``SCRIPT.txt`` file.

See the copyright and license information on the contributed files for
usage and distributions conditions.

API documentation
-----------------

Open the
`../../docs/library_index.html#xml_parser <../../docs/library_index.html#xml_parser>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(xml_parser(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(xml_parser(tester)).

Known issues
------------

When using GNU Prolog as the backend compiler, you may need to to use a
larger default global stack size (see the GNU Prolog documentation on
the environment variable ``GLOBALSZ``).
