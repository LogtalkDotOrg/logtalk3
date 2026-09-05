.. _library_stop_words:

``stop_words``
==============

This library provides predicates for enumerating, recognizing, and
filtering stop words represented as atoms, character lists, or character
code lists. Language providers store canonical lowercase words as
indexed atom facts.

API documentation
-----------------

Open the
`../../apis/library_index.html#stop-words <../../apis/library_index.html#stop-words>`__
link in a web browser.

Loading
-------

To load the library main files and the default English language file,
load the ``loader.lgt`` file:

::

   | ?- logtalk_load(stop_words(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(stop_words(tester)).

Tests load and run smoke tests on all included language files.

Usage
-----

The ``stop_words(Representation, Language)`` object accepts ``atom``,
``chars``, and ``codes`` representations. The language parameter is an
object implementing the ``stop_words_language_protocol`` protocol.

The library includes the following language provider objects:

- ``stopwords_en`` - English
- ``stopwords_pt`` - Portuguese
- ``stopwords_es`` - Spanish
- ``stopwords_fr`` - French
- ``stopwords_de`` - German

To enumerate canonical English stop words as atoms:

::

   | ?- stop_words(atom, stopwords_en)::stop_word(Word).
   Word = 'll'
   ...

To test membership, including ASCII case normalization:

::

   | ?- stop_words(atom, stopwords_en)::is_stop_word('THE').
   yes

To remove stop words while preserving the original values and order of
the remaining tokens:

::

   | ?- stop_words(atom, stopwords_en)::exclude(['The',quick,and,'BROWN',fox], Filtered).
   Filtered = [quick,'BROWN',fox]
   yes

``stop_word/1`` on a language provider enumerates canonical lowercase
atoms. ``stop_word/1`` on the parametric facade enumerates those words
in the configured representation. ``is_stop_word/1`` and ``exclude/2``
lowercase ASCII letters for matching. Tokenization, punctuation
stripping, stemming, and Unicode normalization are the caller's
responsibility.

Stop-word sources and license
-----------------------------

The default language objects are generated from the corresponding
``stopwords-iso`` repositories:

https://github.com/stopwords-iso

Each source snapshot is pinned to a full commit identifier under
``scripts/sources/stopwords-LANGUAGE/``. The source collections and
generated objects are copyright 2016 Gene Diaz and licensed under the
MIT License. Each original license is preserved beside its source
snapshot and copied verbatim into the generated object. The remaining
library files are licensed under Apache-2.0.

The upstream entries are preserved without correction, except for four
malformed non-ASCII entries in the English source (``herse”``,
``himse”``, ``itse”``, and ``myse”``). Their intended reflexive pronouns
already occur in the source, so the malformed duplicates are omitted
from the generated ASCII-only English object.

To update or add a language object, pass the individual repository name
and a full commit identifier to the generic updater. For example:

::

   $ ./scripts/update_stopwords.sh stopwords-pt 84249b7ca37005f8cd19df70d86f44dc1f1936a5

The repository must use the ``stopwords-LANGUAGE`` naming convention and
provide ``stopwords-LANGUAGE.txt`` plus an MIT ``LICENSE`` file. The
generated object and file are named ``stopwords_LANGUAGE``. Language
files other than the default ``stopwords_en.lgt`` file include an
``encoding('UTF-8')`` directive in the first line.

Adding languages
----------------

A language provider implements ``stop_words_language_protocol`` with
indexed lowercase atom facts:

::

   :- object(example_stop_words,
       implements(stop_words_language_protocol)).

       stop_word(a).
       stop_word(the).

   :- end_object.
