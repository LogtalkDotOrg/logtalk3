.. _library_dates:

``dates``
=========

The ``date`` object implements some useful calendar date predicates.

It also provides portable predicates for date-time handling, including:

- Unix epoch conversions (``data_time_to_unix/2``,
  ``unix_to_data_time/2``)
- date-time arithmetic with durations (``add_duration/3``,
  ``subtract_duration/3``, ``duration_between/3``)
- UTC/local conversion using explicit offsets (``utc_to_local/3``,
  ``local_to_utc/3``)
- calendar utilities (``day_of_year/2``, ``week_of_year_iso/2``,
  ``weekday/2``)
- date-time normalization and validation (``normalize_data_time/2``,
  ``valid_data_time/1-2``)

Date-time values are represented using the ``date_time/6`` compound
term:

- ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``

The ``time`` object implements some useful time predicates.

Please note that the functionality of these objects depends on the
chosen Prolog support for accessing the operating system time and date.

API documentation
-----------------

Open the
`../../apis/library_index.html#dates <../../apis/library_index.html#dates>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(dates(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(dates(tester)).
