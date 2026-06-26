.. _library_jwt:

``jwt``
=======

The ``jwt`` library provides compact JWT parsing, JWS signing and
verification, JWK/JWKS key selection, and reusable registered-claim
validation predicates.

The current implementation supports native ``HS256``
signing/verification and OpenSSL-backed ``RS256``/``ES256``
verification. ``ES256`` signatures are converted from JOSE raw
``R || S`` encoding to DER before calling OpenSSL.

This library requires a Prolog backend supporting unbound integer
arithmetic.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(jwt(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(jwt(tester)).

Basic usage
-----------

Decode a compact JWT without verifying its signature:

::

   | ?- jwt::decode(Token, Header, Claims).

Inspect only selected header or claims data:

::

   | ?- jwt::peek_algorithm(Token, Algorithm).

   | ?- jwt::claim(Claims, sub, Subject).

Sign and verify a token using native ``HS256`` support:

::

   | ?- Header = {alg-'HS256', typ-'JWT'},
        Claims = {sub-'123', exp-4102444800},
        jwt::sign(Header, Claims, 'shared-secret', Token, []),
        jwt::verify(Token, 'shared-secret', VerifiedClaims, []).

Validate claims independently from signature verification:

::

   | ?- Policy = [
            claim(iss, expected('https://issuer.example')),
            claim(aud, contains('client-id')),
            claim(exp, time(expiration))
        ],
        jwt::validate_claims(Claims, Policy, [now(1700000001)]).

Verify an asymmetric token using a JWK Set. The library selects a key
matching the JWT header ``alg`` and optional ``kid`` values:

::

   | ?- JWKSet = {keys-[PublicJWK]},
        jwt::verify(Token, JWKSet, Claims, [
            allow_algorithms(['RS256', 'ES256']),
            claim_policy([
                claim(iss, expected('https://issuer.example')),
                claim(aud, contains('client-id'))
            ])
        ]).

Use the ``openssl_executable/1`` option when the OpenSSL command is not
named ``openssl`` or is not found in the default command search path:

::

   | ?- jwt::verify(Token, JWKSet, Claims, [
            openssl_executable('/opt/local/bin/openssl')
        ]).
