.. _library_gravatar:

``gravatar``
============

The ``gravatar`` library provides a portable client for retrieving
Gravatar profile information using the Gravatar REST API. It uses
``http_client`` with the ``http_process_transport`` transport for HTTPS
support and reuses the ``crypto``, ``options``, ``os``, ``json``,
``url``, and ``rest`` libraries.

This library can be used with backend Prolog systems that support
unbound integer arithmetic and the ``sockets`` library: ECLiPSe, SICStus
Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(gravatar(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(gravatar(tester)).

To run the optional live tests against the public Gravatar API, define
the ``LOGTALK_GRAVATAR_API_KEY`` environment variable and load the
``tester_live.lgt`` file:

::

   | ?- logtalk_load(gravatar(tester_live)).

The live tests exercise the account profile for ``pmoura@logtalk.org``
and are skipped automatically when the API key environment variable is
not defined.

Basic usage
-----------

::

   | ?- logtalk_load(gravatar(loader)).
   ...

   | ?- gravatar::profile('user@example.com', Profile).

For authenticated requests, pass an API key explicitly:

::

   | ?- gravatar::profile('user@example.com', Profile, [api_key('...')]).

When the ``api_key/1`` option is omitted, the client uses the
``LOGTALK_GRAVATAR_API_KEY`` environment variable when it is defined. If
neither source is available, the request is sent without authentication.
But this last resort is strongly discouraged.

The decoded profile is returned as a JSON term. Convenience predicates
are available for common profile fields:

::

   | ?- gravatar::profile('user@example.com', Profile),
        gravatar::display_name(Profile, DisplayName),
        gravatar::avatar_url(Profile, AvatarURL).

The supported helpers are ``hash/2``, ``display_name/2``,
``profile_url/2``, ``avatar_url/2``, ``avatar_alt_text/2``,
``location/2``, ``job_title/2``, ``company/2``, ``description/2``,
``pronouns/2``, ``verified_accounts/2``, and ``section_visibility/2``.
The generic ``field/3`` predicate can be used for any top-level profile
field.
