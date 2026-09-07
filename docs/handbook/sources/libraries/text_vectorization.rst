.. _library_text_vectorization:

``text_vectorization``
======================

This library provides deterministic binary, count, term-frequency, and
TF-IDF vectorization. It learns a vocabulary and corpus statistics from
a list of documents and transforms documents into sparse feature
vectors.

Documents are lists of arbitrary ground feature terms. Thus, callers can
use tokens represented as atoms, character lists, or character code
lists, as well as token n-grams, without converting them to a
library-specific format.

API documentation
-----------------

Open the
`../../apis/library_index.html#text-vectorization <../../apis/library_index.html#text-vectorization>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(text_vectorization(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(text_vectorization(tester)).

Usage
-----

Learn a vectorizer and transform a document using the default smoothed
TF-IDF weighting:

::

   | ?- Corpus = [[the,cat,sat], [the,dog,sat], [the,dog,ran]],
        text_vectorizer::learn(Corpus, Vectorizer),
        text_vectorizer::transform(Vectorizer, [the,cat,ran], Vector).
   Vectorizer = text_vectorizer_model(...),
   Vector = [cat-..., ran-..., the-1.0]
   yes

The ``learn_transform/3-4`` predicates learn a vectorizer and transform
the training corpus in one call. The ``transform_all/3`` predicate
transforms a list of documents while preserving document order.
Out-of-vocabulary features are ignored. A document containing no learned
features produces the empty sparse vector.

The learned vocabulary and every sparse ``Feature-Weight`` vector use
standard term order. This provides deterministic results independently
of corpus and document feature order.

Weighting
---------

The ``weighting(Weighting)`` option supports:

- ``binary`` - assigns the integer weight one to each observed feature.
- ``count`` - uses raw feature occurrence counts.
- ``term_frequency`` - divides each count by the number of in-vocabulary
  feature occurrences in the document.
- ``tf_idf(raw)`` - multiplies raw occurrence counts by IDF weights.
  This is the default.
- ``tf_idf(relative)`` - multiplies relative term frequencies by IDF
  weights.
- ``tf_idf(sublinear)`` - uses ``1 + log(Count)`` as the term-frequency
  factor.

The ``idf(smooth)`` default computes:

::

   IDF = log((1 + DocumentCount) / (1 + DocumentFrequency)) + 1

The ``idf(classic)`` option computes:

::

   IDF = log(DocumentCount / DocumentFrequency)

Classic IDF assigns zero to features occurring in every corpus document.
Such zero-weight entries are omitted from sparse vectors.

Options
-------

The ``learn/3`` and ``learn_transform/4`` predicates accept these
options:

- ``weighting(Weighting)``, defaulting to ``tf_idf(raw)``.
- ``idf(smooth|classic)``, defaulting to ``smooth``.
- ``normalization(none|l1|l2)``, defaulting to ``none``.
- ``minimum_document_frequency(PositiveInteger)``, defaulting to ``1``.
- ``maximum_document_frequency(all|PositiveInteger)``, defaulting to
  ``all``.
- ``maximum_features(all|PositiveInteger)``, defaulting to ``all``.

Document-frequency bounds are inclusive. An integer maximum document
frequency cannot exceed the number of training documents, and the
minimum cannot exceed the effective maximum.

When ``maximum_features(N)`` is used, features are selected by
decreasing total corpus occurrence count, with standard term order
breaking ties. Selected features are then restored to standard term
order.

L1 normalization divides weights by their absolute sum. L2 normalization
divides weights by their Euclidean norm. Empty vectors remain empty.

Pipeline integration
--------------------

Text preprocessing remains explicit and composable. For example, a
caller can tokenize, remove stop words, lemmatize, and generate n-grams
before learning or applying a vectorizer. The library does not depend on
a language, tokenizer, normalization profile, stop-word provider,
stemmer, or lemmatizer.

Model representation
--------------------

Learned models use the portable ground representation:

::

   text_vectorizer_model(Features, Diagnostics)

``Features`` is a list of
``feature(Feature, DocumentFrequency, InverseDocumentFrequency)`` terms
in standard term order. Diagnostics record the effective options,
document count, vocabulary size, and total number of input feature
occurrences. The model can therefore be persisted using ordinary term
I/O.

Limitations
-----------

The library currently provides sparse vectors only. Dense matrix
conversion, BM25 weighting, hashing vectorization, incremental fitting,
and mutable vocabularies are not implemented. The full training corpus
is processed in memory.
