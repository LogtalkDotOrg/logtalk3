________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the License);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`http_static_files`
===================

The `http_static_files` library provides a router-agnostic helper for serving
existing files from a configured document root using normalized `http` request
and response terms.

This library resolves a relative request path against a document root, canonicalizes
the joined target path using `os::absolute_file_name/2`, rejects canonicalized
escapes with the same outward `404 Not Found` response used for missing files,
falls back to configurable index files for directory targets, guesses the
response media type using `mime_types`, emits `ETag`, `Last-Modified`, and
`Accept-Ranges` metadata, and returns file-backed response bodies that
`http_server` can stream efficiently.


API documentation
-----------------

Open the [../../apis/library_index.html#http_static_files](../../apis/library_index.html#http_static_files)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

    | ?- logtalk_load(http_static_files(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

    | ?- logtalk_load(http_static_files(tester)).


Current scope
-------------

The current library provides one public object with two public predicates:

- `serve/4`
- `serve/5`

Supported options are:

- `index_files(IndexFiles)`
- `mime_types_strict(Boolean)`
- `cache_control(Directives)`
- `expires(Expires)`

Default options are:

- `index_files(['index.html', 'index.htm'])`
- `mime_types_strict(false)`
- `cache_control([])`
- `expires(none)`

Supported features:

- `GET` and `HEAD`
- `200 OK` file responses
- canonical docroot-prefix checks after path normalization
- weak `ETag` and `Last-Modified` validators derived from file size and
    modification time
- `304 Not Modified` responses for matching `If-None-Match` and
    `If-Modified-Since` requests, with `If-None-Match` taking precedence
- single `bytes=` ranges with `206 Partial Content`
- `416 Range Not Satisfiable` for malformed, unsupported multi-range, and
    unsatisfiable range requests
- `If-Range` fallback to full `200 OK` responses when the validator does not
    match or is only weakly equal
- `404 Not Found` for missing or unsafe paths
- `405 Method Not Allowed` for other methods
- index-file lookup for directory targets
- precompressed `.br` and `.gz` asset negotiation driven by `Accept-Encoding`
- `Vary: Accept-Encoding` responses when negotiated precompressed variants exist
- MIME type and content-encoding guessing
- configurable `Cache-Control` and `Expires` response headers

For directory listing, see the `http_directory_listing` library.

Cache-policy configuration uses these option values:

- `cache_control(Directives)` where `Directives` is a list containing any of
    `public`, `private`, `no_cache`, `no_store`, `no_transform`,
    `must_revalidate`, `proxy_revalidate`, `immutable`, `max_age(Seconds)`,
    `s_maxage(Seconds)`, `stale_while_revalidate(Seconds)`,
    `stale_if_error(Seconds)`, or `extension(Directive)`
- `expires(none)` to omit the header, `expires(Seconds)` for a relative expiry
    from the current system time, or `expires(date_time(Year,Month,Day,Hour,Minute,Second))`
    for an absolute expiry time

Current validator and date handling is intentionally conservative:

- `ETag` values are weak validators of the form `W/"Size-ModifiedTime"`
- HTTP-date formatting, date validation, and Unix-time conversion delegate to
    the `dates` library; request parsing remains limited to the IMF-fixdate
    form generated for `Last-Modified`
- `If-Range` entity-tag evaluation uses strong comparison semantics, so the
    generated weak `ETag` values never authorize partial responses; matching
    `Last-Modified` dates can still do so
- `If-Range` mismatches fall back to the full `200 OK` response before range
    validation, so malformed or unsupported range syntax is ignored in that case
- precompressed-asset negotiation currently recognizes `.br` and `.gz` sibling
    files and selects between them using `Accept-Encoding` quality values,
    otherwise falling back to the identity representation
- path sandboxing is based on canonicalized absolute-path prefix checks; no
    separate cross-backend symlink-policy layer has been added yet
