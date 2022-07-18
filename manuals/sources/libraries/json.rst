.. _library_json:

``json``
========

The ``json`` library provides predicates for parsing and generating data
in the JSON format based on the specification and standard found at:

-  https://tools.ietf.org/html/rfc8259
-  https://www.ecma-international.org/publications-and-standards/standards/ecma-404/

It includes a parametric object whose parameter indicates the preferred
representation for decoded JSON text strings (``atom``, ``chars``, or
``codes``).

API documentation
-----------------

Open the
`../../docs/library_index.html#json <../../docs/library_index.html#json>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(json(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(json(tester)).

Some of the sample JSON test files are based on examples published at:

https://www.json.org/

Representation
--------------

The following choices of syntax has been made to represent JSON elements
as terms:

-  JSON objects are represented using curly-bracketed terms,
   ``{Pairs}``, where each pair uses the representation ``Key-Value``.

-  Arrays are represented using lists.

-  Text strings can be represented as atoms, ``chars(List)``, or
   ``codes(List)``. The default when decoding is to use atoms when using
   the ``json`` object. To decode text strings into lists of chars or
   codes, use the ``json/1`` with the parameter bound to ``chars`` or
   ``codes``. For example:

   ::

        | ?- json::parse(codes([34,104,101,108,108,111,34]), Term).
        Term = hello
        yes

        | ?- json(atom)::parse(codes([34,104,101,108,108,111,34]), Term).
        Term = hello
        yes

        | ?- json(chars)::parse(codes([34,104,101,108,108,111,34]), Term).
        Term = chars([h,e,l,l,o])
        yes

        | ?- json(codes)::parse(codes([34,104,101,108,108,111,34]), Term).
        Term = codes([104,101,108,108,111])
        yes

-  The JSON values ``false``, ``true`` and ``null`` are represented by,
   respectively, the ``@false``, ``@true`` and ``@null`` compound terms.

The following table exemplifies the term equivalents of JSON elements
(with) JSON strings being represented as atoms:

========================= =========================
JSON                      term
========================= =========================
[1,2]                     [1,2]
true                      @true
false                     @false
null                      @null
-1                        -1
[1.2345]                  [1.2345]
[]                        []
[2147483647]              [2147483647]
[0]                       [0]
[1234567890123456789]     [1234567890123456789]
[false]                   [@false]
[-2147483648]             [-2147483648]
{"a":null,"foo":"bar"}    {a-@null, foo-bar}
[2.225073858507201e-308]  [2.225073858507201e-308]
[0,1]                     [0,1]
[2.2250738585072014e-308] [2.2250738585072014e-308]
[1.7976931348623157e+308] [1.7976931348623157e+308]
[0.0]                     [0.0]
[4294967295]              [4294967295]
[-1234567890123456789]    [-1234567890123456789]
["foo"]                   [foo]
[1]                       [1]
[null]                    [@null]
[-1.2345]                 [-1.2345]
[5.0e-324]                [5.0e-324]
[-1]                      [-1]
[true]                    [@true]
[9223372036854775807]     [9223372036854775807]
{"foo":"bar"}             {foo-bar}
{}                        {}
========================= =========================

Encoding
--------

Encoding is accomplished using the ``generate/2`` predicate. For
example:

::

   | ?- json::generate(codes(Encoding), [a,{b-c}]).
   Encoding = [91,34,97,34,44,123,34,98,34,58,34,99,34,125,93]
   yes

Alternatively:

::

   | ?- json::generate(chars(Encoding), [a,{b-c}]).
   Encoding = ['[','"',a,'"',',','{','"',b,'"',:,'"',c,'"','}',']']
   Yes

   | ?- json::generate(atom(Encoding), [a,{b-c}]).
   Encoding = '["a",{"b":"c"}]'
   Yes

Notice that ``generate/2`` takes, as second argument, a Prolog term that
corresponds to the JSON Syntax in Prolog and produces the corresponding
JSON output in the format specified in the first argument:
(``codes(Variable)``, ``stream(Stream)``, ``file(File)``,
``chars(Variable)`` or ``atom(Variable)``).

Decoding
--------

Decoding is accomplished using the ``parse/2`` predicate. For example,
to decode a given json file:

::

   | ?- json::parse(file('simple/roundtrip_array_obj_array.json'), Term).
   Term = [{a-[b]}]
   yes

The ``parse/2`` predicate first argument must indicate the input source
(``codes(Source)``, ``stream(Source)``, ``file(Source)``,
``chars(Source)`` or ``atom(Source)``) containing a JSON payload to be
decoded into the Prolog term in the second argument.
