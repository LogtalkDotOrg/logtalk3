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
existing files from a configured document root using normalized `http_core`
request and response terms.

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
- `mime_type_overrides(Overrides)`
- `fallback_file(Fallback)`
- `cache_control(Directives)`
- `expires(Expires)`
- `content_disposition(Disposition)`

Default options are:

- `index_files(['index.html', 'index.htm'])`
- `mime_types_strict(false)`
- `mime_type_overrides([])`
- `fallback_file(none)`
- `cache_control([])`
- `expires(none)`
- `content_disposition(none)`

Supported features:

- `GET`, `HEAD`, and `OPTIONS`
- `200 OK` file responses
- `204 No Content` `OPTIONS` responses with `Allow: GET, HEAD, OPTIONS`
- canonical docroot-prefix checks after path normalization
- weak `ETag` and `Last-Modified` validators derived from file size and
    modification time
- `412 Precondition Failed` responses for failing `If-Match` and
    `If-Unmodified-Since` request preconditions
- `304 Not Modified` responses for matching `If-None-Match` and
    `If-Modified-Since` requests, with `If-None-Match` taking precedence
- single `bytes=` ranges with `206 Partial Content`
- `416 Range Not Satisfiable` for malformed, unsupported multi-range, and
    unsatisfiable range requests
- `If-Range` fallback to full `200 OK` responses when the validator does not
    match or is only weakly equal
- `406 Not Acceptable` when `Accept-Encoding` rejects the identity
    representation and no acceptable precompressed variant is available
- `404 Not Found` for missing or unsafe paths
- custom fallback file responses for missing paths that resolve safely inside
    the document root
- `405 Method Not Allowed` for other methods with `Allow: GET, HEAD, OPTIONS`
- `301 Moved Permanently` trailing-slash redirects for directory targets
- index-file lookup for directory targets
- precompressed `.br` and `.gz` asset negotiation driven by `Accept-Encoding`
- `Vary: Accept-Encoding` responses when negotiated precompressed variants exist
- MIME type and content-encoding guessing
- per-extension and per-path MIME type overrides on top of `mime_types`
- configurable `Cache-Control` and `Expires` response headers, also shared by
    `OPTIONS` responses when configured
- configurable `Content-Disposition` response headers for inline or attachment
    file delivery

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
- `mime_type_overrides(Overrides)` where `Overrides` is a list containing
    `extension(Extension, MediaType)` or `path(Path, MediaType)` terms;
    path overrides match the served document-root relative file path and take
    precedence over extension overrides
- `fallback_file(none)` to omit custom fallback handling,
    `fallback_file(not_found(Path))` to serve a document-root relative file
    with `404 Not Found` status for missing targets, or
    `fallback_file(spa(Path))` to serve a document-root relative file with
    `200 OK` status for SPA client-side routes; fallback files are only
    considered after the requested path has resolved safely inside the document
    root
- `content_disposition(none)` to omit the header, `content_disposition(inline)`
    or `content_disposition(attachment)` for disposition-only values, and
    `content_disposition(inline(Filename))` or
    `content_disposition(attachment(Filename))` to include a quoted
    `filename` parameter

Current validator and date handling is intentionally conservative:

- `ETag` values are weak validators of the form `W/"Size-ModifiedTime"`
- HTTP-date formatting, date validation, and Unix-time conversion delegate to
    the `dates` library; request parsing remains limited to the IMF-fixdate
    form generated for `Last-Modified`
- `If-Match` entity-tag evaluation uses strong comparison semantics, so the
    generated weak `ETag` values only satisfy the wildcard `*` precondition
- `If-Range` entity-tag evaluation uses strong comparison semantics, so the
    generated weak `ETag` values never authorize partial responses; matching
    `Last-Modified` dates can still do so
- `If-Range` mismatches fall back to the full `200 OK` response before range
    validation, so malformed or unsupported range syntax is ignored in that case
- precompressed-asset negotiation currently recognizes `.br` and `.gz` sibling
    files and selects between them using `Accept-Encoding` quality values,
    otherwise falling back to the identity representation when it remains
    acceptable
- path sandboxing is based on canonicalized absolute-path prefix checks; no
    separate cross-backend symlink-policy layer has been added yet
