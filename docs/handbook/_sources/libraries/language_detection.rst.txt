.. _library_language_detection:

``language_detection``
======================

This library provides lightweight language detection for text
represented as atoms, character lists, or character code lists. It
currently supports the most common languages and returns the
corresponding ISO 639-1 language codes. It requires a backend with
Unicode support.

API documentation
-----------------

Open the
`../../apis/library_index.html#language-detection <../../apis/library_index.html#language-detection>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(language_detection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(language_detection(tester)).

Tests cover all included languages and detection strategies using the
``atom``, ``chars``, and ``codes`` text representations.

Usage
-----

The ``language_detector(Representation, Strategy)`` parametric object
accepts the ``atom``, ``chars``, and ``codes`` representations. The
strategy parameter must be an object implementing the
``language_detection_strategy_protocol`` protocol.

To detect the most likely language:

::

   | ?- language_detector(atom, ngram_language_detector)::detect(
            'The quick brown fox jumps over the lazy dog.', Language
        ).
   Language = en
   yes

To also return a relative detection score:

::

   | ?- language_detector(atom, hybrid_language_detector)::detect(
            'Questo testo è scritto in italiano.', Language, Score,
            [min_length(10)]
        ).
   Language = it,
   Score = ...
   yes

The ``detect_all/2-3`` predicates return all candidate languages ordered
by decreasing score and then by increasing language code. The scores are
relative to the selected candidate set; they are not calibrated
probabilities and should not be compared across different candidate
sets.

The input is normalized to NFC, converted to lowercase, stripped of
control characters, and normalized for whitespace before detection.
Diacritics are preserved because they provide useful language evidence.

Strategies
----------

The library includes the following strategy objects:

- ``ngram_language_detector`` compares character-trigram count vectors
  using cosine similarity.
- ``stopword_language_detector`` tokenizes the input and scores
  discriminative stop-word matches.
- ``script_language_detector`` scores candidates using the Unicode
  scripts observed in the input. Languages sharing the same script
  receive the same script evidence and are therefore expected to tie
  when used alone.
- ``hybrid_language_detector`` first combines the n-gram and stop-word
  scores using weights of 0.75 and 0.25, respectively, and then combines
  the result with script scores using weights of 0.80 and 0.20. When all
  strategies find evidence, the effective weights are therefore 0.60 for
  n-grams, 0.20 for stop words, and 0.20 for scripts. When either
  lexical or script evidence is unavailable, the available scores are
  returned unchanged.

Strategy selection is performed by the second object parameter. It is
not an option of the ``language_detector(Representation, Strategy)``
object.

Script analysis
---------------

The ``language_detection_scripts`` object exposes the script analysis
used by the script strategy. It accepts normalized character codes and
returns all observed Unicode scripts with ratios ordered by decreasing
ratio:

::

   | ?- atom_codes('abc éè', Codes),
        language_detection_scripts::script_ratios(Codes, Ratios).
   Ratios = ['Latin'-0.8333333333333334, 'Common'-0.16666666666666666]
   yes

Script names are the values provided by the Unicode Character Database,
such as ``'Latin'``, ``'Cyrillic'``, and ``'Arabic'``. The
``scripts_to_languages/2`` predicate maps one or more meaningful scripts
to the sorted set of compatible registered languages:

::

   | ?- language_detection_scripts::scripts_to_languages(['Cyrillic'], Languages).
   Languages = [bg, ru, uk]
   yes

The script strategy ignores the ``'Common'``, ``'Inherited'``,
``'Unknown'``, and ``'Zzzz'`` values when scoring. For mixed-script
text, evidence from every other observed script is retained. If no
meaningful mapped script is observed, the strategy returns no scores and
detection fails normally.

Options
-------

The ``detect/4`` and ``detect_all/3`` predicates accept:

- ``min_length(N)``, defaulting to ``20``, sets the minimum normalized
  text length.
- ``candidates(all|Languages)``, defaulting to ``all``, selects either
  all built-in profiles or a nonempty list of supported ISO 639-1
  language codes.
- ``min_score(Score)``, defaulting to ``0.30``, sets the minimum best
  relative score.
- ``min_margin(Margin)``, defaulting to ``0.05``, sets the minimum
  difference between the two highest scores.

Duplicate candidate codes, unsupported candidate codes, duplicate
options, and invalid option values are rejected.

The ``detect/2-4`` predicates fail when the normalized input is shorter
than the configured minimum or when the available evidence does not
satisfy the score and margin thresholds. The ``detect_all/2-3``
predicates return an empty list in those cases.

Language profiles
-----------------

The library includes the several profile objects named
``XX_language_profile`` in the ``profiles`` sub-directory where ``XX``
is the ISO language code.

The character-trigram vectors are deterministically derived from the
vendored stop-word inventories. Words are padded with spaces before
trigram extraction so that word boundaries contribute evidence. The
stop-word source files record their upstream repositories, commits,
licenses, and generated entry counts.

Adding strategies and profiles
------------------------------

A custom strategy implements the
``language_detection_strategy_protocol`` protocol. Its ``scores/3``
predicate receives normalized character codes and a validated list of
candidate language codes and returns one ``Language-Score`` pair for
every candidate, or an empty list when it finds no evidence.

A custom profile implements the ``language_profile_protocol`` protocol.
Profiles are registered by defining the
``language_profiles::custom_profile/2`` multifile predicate. For
example:

::

   :- multifile(language_profiles::custom_profile/2).
   language_profiles::custom_profile(it, it_custom_language_profile).

The profile protocol and registry allow replacing the bundled vectors
with corpus-derived profiles without changing the detector or strategy
APIs.

A custom profile can override or add its expected scripts by defining
the ``language_detection_scripts::custom_language_scripts/2`` multifile
predicate:

::

   :- multifile(language_detection_scripts::custom_language_scripts/2).
   language_detection_scripts::custom_language_scripts(sr, ['Cyrillic', 'Latin']).

Limitations
-----------

Accuracy decreases for short, code-switched, transliterated, or highly
specialized text. Only the most common languages are currently included.
The bundled n-gram profiles are intentionally lightweight and are not a
substitute for profiles trained and evaluated on balanced corpora.
Script-run segmentation, calibrated probabilities, and runtime profile
training are outside the scope of this library.
