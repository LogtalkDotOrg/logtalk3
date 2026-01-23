%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    cover(url(_)).

    % valid/1 tests

    test(valid_1_01, true) :-
        url::valid("http://example.com").

    test(valid_1_02, true) :-
        url::valid("https://example.com/path/to/resource").

    test(valid_1_03, true) :-
        url::valid("http://example.com/path/to/file.html").

    test(valid_1_04, true) :-
        url::valid("http://example.com/path/to/file.html?param1=value1&param2=value2").

    test(valid_1_05, true) :-
        url::valid("http://example.com/path/to/file.html?param=value#section").

    test(valid_1_06, true) :-
        url::valid("http://192.168.1.1/path/to/resource").

    test(valid_1_07, true) :-
        url::valid("ftp://ftp.example.com/pub/file.zip").

    test(valid_1_08, true) :-
        url::valid("rtsp://media.example.com/stream").

    test(valid_1_09, true) :-
        url::valid("file:///home/user/document.txt").

    test(valid_1_10, true) :-
        url::valid("mailto:user@example.com").

    test(valid_1_11, false) :-
        url::valid("invalid url").

    test(valid_1_12, false) :-
        url::valid("http:example.com").

    % parse/2 tests

    test(parse_2_01, true(Components == [scheme("http"), authority("example.com"), path(""), query(""), fragment("")])) :-
        url::parse("http://example.com", Components).

    test(parse_2_02, true(Components == [scheme("https"), authority("example.com"), path("/path/to"), query(""), fragment("")])) :-
        url::parse("https://example.com/path/to", Components).

    test(parse_2_03, true(Components == [scheme("http"), authority("example.com"), path("/path/to/file.html"), query(""), fragment("")])) :-
        url::parse("http://example.com/path/to/file.html", Components).

    test(parse_2_04, true(Components == [scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param1=value1&param2=value2"), fragment("")])) :-
        url::parse("http://example.com/path/to/file.html?param1=value1&param2=value2", Components).

    test(parse_2_05, true(Components == [scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param=value"), fragment("section")])) :-
        url::parse("http://example.com/path/to/file.html?param=value#section", Components).

    test(parse_2_06, true(Components == [scheme("http"), authority("192.168.1.1"), path("/path/to/resource"), query(""), fragment("")])) :-
        url::parse("http://192.168.1.1/path/to/resource", Components).

    test(parse_2_07, true(Components == [scheme("ftp"), authority("ftp.example.com"), path("/pub/file.zip")])) :-
        url::parse("ftp://ftp.example.com/pub/file.zip", Components).

    test(parse_2_08, true(Components == [scheme("rtsp"), authority("media.example.com"), path("/stream")])) :-
        url::parse("rtsp://media.example.com/stream", Components).

    % File URI tests
    test(parse_2_09, true) :-
        url::parse("file:///home/user/document.txt", _Components).

    % Mailto URI tests
    test(parse_2_10, true) :-
        url::parse("mailto:user@example.com", _Components).

    % normalize/2 tests

    test(normalize_2_01, true(NormalizedURL == "http://example.com")) :-
        url::normalize("HTTP://example.com", NormalizedURL).

    test(normalize_2_02, true(NormalizedURL == "http://example.com/path/")) :-
        url::normalize("http://example.com/path/", NormalizedURL).

    test(normalize_2_03, true(NormalizedURL == "http://example.com/path/to/resource")) :-
        url::normalize("http://example.com/path/to/resource", NormalizedURL).

    test(normalize_2_04, true(NormalizedURL == "http://example.com/path/to/resource")) :-
        url::normalize("http://example.com/path/to/../to/resource", NormalizedURL).

    test(normalize_2_05, true(NormalizedURL == "http://example.com/resource")) :-
        url::normalize("http://example.com/path/../resource", NormalizedURL).

    test(normalize_2_06, true(NormalizedURL == "http://example.com/path/to/resource?query=value#fragment")) :-
        url::normalize("http://example.com/path/to/resource?query=value#fragment", NormalizedURL).

    % Additional tests for edge cases

    test(url_edge_case_01, true) :-
        url::valid("http://example.com:8080/path").

    test(url_edge_case_02, true) :-
        url::valid("http://user:password@example.com/path").

    test(url_edge_case_03, true) :-
        url::valid("http://example.com/path%20with%20spaces").

    test(url_edge_case_04, true) :-
        url::valid("http://example.com/path?query=value%20with%20spaces").

    test(url_edge_case_05, true) :-
        url::valid("http://example.com/path?query=value#fragment%20with%20spaces").

    test(url_edge_case_06, true) :-
        url::valid("mailto:user+tag@example.com").

    test(url_edge_case_07, true) :-
        url::valid("file:///c:/Windows/System32/").

    test(url_edge_case_08, true) :-
        url::valid("https://example.com/").

    test(url_edge_case_09, true) :-
        url::valid("https://example.com/?").

    test(url_edge_case_10, true) :-
        url::valid("https://example.com/#").

    % New URL schemes tests

    test(url_scheme_ldap_01, true) :-
        url::valid("ldap://ldap.example.com/dc=example,dc=com").

    test(url_scheme_ldap_02, true) :-
        url::valid("ldap://ldap.example.com/dc=example,dc=com?cn").

    test(url_scheme_news_01, true) :-
        url::valid("news:comp.infosystems.www.servers.unix").

    test(url_scheme_news_02, true) :-
        url::valid("news:example.message-id@example.com").

    test(url_scheme_tel_01, true) :-
        url::valid("tel:+1-816-555-1212").

    test(url_scheme_tel_02, true) :-
        url::valid("tel:+44(0)2012345678").

    test(url_scheme_telnet_01, true) :-
        url::valid("telnet://192.0.2.16:80/").

    test(url_scheme_telnet_02, true) :-
        url::valid("telnet://example.com/").

    test(url_scheme_urn_01, true) :-
        url::valid("urn:oasis:names:specification:docbook:dtd:xml:4.1.2").

    test(url_scheme_urn_02, true) :-
        url::valid("urn:isbn:0451450523").

    % RFC3986 edge cases

    test(rfc3986_empty_authority, true) :-
        url::valid("file:///path/to/file").

    test(rfc3986_port_number, true) :-
        url::valid("http://example.com:8080/path").

    test(rfc3986_empty_path, true) :-
        url::valid("http://example.com").

    test(rfc3986_query_only, true) :-
        url::valid("http://example.com?query").

    test(rfc3986_fragment_only, true) :-
        url::valid("http://example.com#fragment").

    test(rfc3986_unreserved_chars, true) :-
        url::valid("http://example.com/path-with_under.tilde~").

    test(rfc3986_sub_delims, true) :-
        url::valid("http://example.com/path!$&'()*+,;=test").

    test(rfc3986_case_insensitive_scheme_01, true) :-
        url::valid("HTTP://example.com/").

    test(rfc3986_case_insensitive_scheme_02, true) :-
        url::valid("HtTp://example.com/").

    test(rfc3986_case_insensitive_scheme_03, true) :-
        url::valid("HTTPS://example.com/").

    test(rfc3986_case_insensitive_scheme_04, true) :-
        url::valid("FTP://example.com/").

    test(rfc3986_case_insensitive_scheme_05, true) :-
        url::valid("MAILTO:user@example.com").

    % Parse URL tests for new schemes

    test(parse_ldap_01, true(Components == [scheme("ldap"), authority("ldap.example.com"), path("/dc=example,dc=com"), query("")])) :-
        url::parse("ldap://ldap.example.com/dc=example,dc=com", Components).

    test(parse_news_01, true(Components == [scheme("news"), identifier("comp.infosystems.www.servers.unix")])) :-
        url::parse("news:comp.infosystems.www.servers.unix", Components).

    test(parse_tel_01, true(Components == [scheme("tel"), number("+1-816-555-1212")])) :-
        url::parse("tel:+1-816-555-1212", Components).

    test(parse_telnet_01, true(Components == [scheme("telnet"), authority("192.0.2.16:80"), path("/")])) :-
        url::parse("telnet://192.0.2.16:80/", Components).

    test(parse_urn_01, true(Components == [scheme("urn"), identifier("oasis:names:specification:docbook:dtd:xml:4.1.2")])) :-
        url::parse("urn:oasis:names:specification:docbook:dtd:xml:4.1.2", Components).

    % Additional URL schemes tests

    % FTPS (FTP Secure)
    test(url_scheme_ftps_01, true) :-
        url::valid("ftps://ftp.example.com/pub/file.zip").

    test(parse_ftps_01, true(Components == [scheme("ftps"), authority("ftp.example.com"), path("/pub/file.zip")])) :-
        url::parse("ftps://ftp.example.com/pub/file.zip", Components).

    % SFTP (SSH File Transfer Protocol)
    test(url_scheme_sftp_01, true) :-
        url::valid("sftp://user@server.example.com/path/to/file").

    test(parse_sftp_01, true(Components == [scheme("sftp"), authority("user@server.example.com"), path("/path/to/file")])) :-
        url::parse("sftp://user@server.example.com/path/to/file", Components).

    % SSH (Secure Shell)
    test(url_scheme_ssh_01, true) :-
        url::valid("ssh://user@host.example.com/").

    test(parse_ssh_01, true(Components == [scheme("ssh"), authority("user@host.example.com"), path("/")])) :-
        url::parse("ssh://user@host.example.com/", Components).

    % LDAPS (LDAP Secure)
    test(url_scheme_ldaps_01, true) :-
        url::valid("ldaps://ldap.example.com/dc=example,dc=com").

    test(parse_ldaps_01, true(Components == [scheme("ldaps"), authority("ldap.example.com"), path("/dc=example,dc=com"), query("")])) :-
        url::parse("ldaps://ldap.example.com/dc=example,dc=com", Components).

    % RTMP (Real-Time Messaging Protocol)
    test(url_scheme_rtmp_01, true) :-
        url::valid("rtmp://media.example.com/live/stream").

    test(parse_rtmp_01, true(Components == [scheme("rtmp"), authority("media.example.com"), path("/live/stream")])) :-
        url::parse("rtmp://media.example.com/live/stream", Components).

    % MMS (Microsoft Media Server)
    test(url_scheme_mms_01, true) :-
        url::valid("mms://media.example.com/stream").

    test(parse_mms_01, true(Components == [scheme("mms"), authority("media.example.com"), path("/stream")])) :-
        url::parse("mms://media.example.com/stream", Components).

    % JDBC (Java Database Connectivity)
    test(url_scheme_jdbc_01, true) :-
        url::valid("jdbc://localhost:3306/database").

    test(parse_jdbc_01, true(Components == [scheme("jdbc"), authority("localhost:3306"), path("/database")])) :-
        url::parse("jdbc://localhost:3306/database", Components).

    % MongoDB
    test(url_scheme_mongodb_01, true) :-
        url::valid("mongodb://localhost:27017/mydb").

    test(parse_mongodb_01, true(Components == [scheme("mongodb"), authority("localhost:27017"), path("/mydb")])) :-
        url::parse("mongodb://localhost:27017/mydb", Components).

    % MySQL
    test(url_scheme_mysql_01, true) :-
        url::valid("mysql://user:password@localhost:3306/database").

    test(parse_mysql_01, true(Components == [scheme("mysql"), authority("user:password@localhost:3306"), path("/database")])) :-
        url::parse("mysql://user:password@localhost:3306/database", Components).

    % PostgreSQL
    test(url_scheme_postgresql_01, true) :-
        url::valid("postgresql://localhost:5432/mydb").

    test(parse_postgresql_01, true(Components == [scheme("postgresql"), authority("localhost:5432"), path("/mydb")])) :-
        url::parse("postgresql://localhost:5432/mydb", Components).

    % WebSocket (ws)
    test(url_scheme_ws_01, true) :-
        url::valid("ws://example.com/socket").

    test(parse_ws_01, true(Components == [scheme("ws"), authority("example.com"), path("/socket"), query(""), fragment("")])) :-
        url::parse("ws://example.com/socket", Components).

    % WebSocket Secure (wss)
    test(url_scheme_wss_01, true) :-
        url::valid("wss://example.com/socket").

    test(parse_wss_01, true(Components == [scheme("wss"), authority("example.com"), path("/socket"), query(""), fragment("")])) :-
        url::parse("wss://example.com/socket", Components).

    % Gopher
    test(url_scheme_gopher_01, true) :-
        url::valid("gopher://gopher.example.com/").

    test(parse_gopher_01, true(Components == [scheme("gopher"), authority("gopher.example.com"), path("/")])) :-
        url::parse("gopher://gopher.example.com/", Components).

    % NNTP (Network News Transfer Protocol)
    test(url_scheme_nntp_01, true) :-
        url::valid("nntp://news.example.com/").

    test(parse_nntp_01, true(Components == [scheme("nntp"), authority("news.example.com"), path("/")])) :-
        url::parse("nntp://news.example.com/", Components).

    % Git
    test(url_scheme_git_01, true) :-
        url::valid("git://github.com/user/repo.git").

    test(parse_git_01, true(Components == [scheme("git"), authority("github.com"), path("/user/repo.git")])) :-
        url::parse("git://github.com/user/repo.git", Components).

    % generate/2 tests

    test(generate_2_01, true(URL == "http://example.com")) :-
        url::generate([scheme("http"), authority("example.com"), path(""), query(""), fragment("")], URL).

    test(generate_2_02, true(URL == "https://example.com/path/to")) :-
        url::generate([scheme("https"), authority("example.com"), path("/path/to"), query(""), fragment("")], URL).

    test(generate_2_03, true(URL == "http://example.com/path/to/file.html")) :-
        url::generate([scheme("http"), authority("example.com"), path("/path/to/file.html"), query(""), fragment("")], URL).

    test(generate_2_04, true(URL == "http://example.com/path/to/file.html?param1=value1&param2=value2")) :-
        url::generate([scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param1=value1&param2=value2"), fragment("")], URL).

    test(generate_2_05, true(URL == "http://example.com/path/to/file.html?param=value#section")) :-
        url::generate([scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param=value"), fragment("section")], URL).

    test(generate_2_06, true(URL == "ftp://ftp.example.com/pub/file.zip")) :-
        url::generate([scheme("ftp"), authority("ftp.example.com"), path("/pub/file.zip")], URL).

    test(generate_2_07, true(URL == "mailto:user@example.com")) :-
        url::generate([scheme("mailto"), address("user@example.com")], URL).

    test(generate_2_08, true(URL == "news:comp.infosystems.www.servers.unix")) :-
        url::generate([scheme("news"), identifier("comp.infosystems.www.servers.unix")], URL).

    test(generate_2_09, true(URL == "tel:+1-816-555-1212")) :-
        url::generate([scheme("tel"), number("+1-816-555-1212")], URL).

    test(generate_2_10, true(URL == "urn:oasis:names:specification:docbook:dtd:xml:4.1.2")) :-
        url::generate([scheme("urn"), identifier("oasis:names:specification:docbook:dtd:xml:4.1.2")], URL).

    test(generate_2_11, true(URL == "ws://example.com/socket")) :-
        url::generate([scheme("ws"), authority("example.com"), path("/socket"), query(""), fragment("")], URL).

    test(generate_2_12, true(URL == "wss://example.com/socket")) :-
        url::generate([scheme("wss"), authority("example.com"), path("/socket"), query(""), fragment("")], URL).

    % Roundtrip tests (parse then generate)

    test(roundtrip_2_01, true(URL == "http://example.com")) :-
        url::parse("http://example.com", Components),
        url::generate(Components, URL).

    test(roundtrip_2_02, true(URL == "https://example.com/path/to")) :-
        url::parse("https://example.com/path/to", Components),
        url::generate(Components, URL).

    test(roundtrip_2_03, true(URL == "http://example.com/path/to/file.html")) :-
        url::parse("http://example.com/path/to/file.html", Components),
        url::generate(Components, URL).

    test(roundtrip_2_04, true(URL == "http://example.com/path/to/file.html?param1=value1&param2=value2")) :-
        url::parse("http://example.com/path/to/file.html?param1=value1&param2=value2", Components),
        url::generate(Components, URL).

    test(roundtrip_2_05, true(URL == "http://example.com/path/to/file.html?param=value#section")) :-
        url::parse("http://example.com/path/to/file.html?param=value#section", Components),
        url::generate(Components, URL).

    test(roundtrip_2_06, true(URL == "ftp://ftp.example.com/pub/file.zip")) :-
        url::parse("ftp://ftp.example.com/pub/file.zip", Components),
        url::generate(Components, URL).

    test(roundtrip_2_07, true(URL == "mailto:user@example.com")) :-
        url::parse("mailto:user@example.com", Components),
        url::generate(Components, URL).

    test(roundtrip_2_08, true(URL == "news:comp.infosystems.www.servers.unix")) :-
        url::parse("news:comp.infosystems.www.servers.unix", Components),
        url::generate(Components, URL).

    test(roundtrip_2_09, true(URL == "tel:+1-816-555-1212")) :-
        url::parse("tel:+1-816-555-1212", Components),
        url::generate(Components, URL).

    test(roundtrip_2_10, true(URL == "urn:oasis:names:specification:docbook:dtd:xml:4.1.2")) :-
        url::parse("urn:oasis:names:specification:docbook:dtd:xml:4.1.2", Components),
        url::generate(Components, URL).

    test(roundtrip_2_11, true(URL == "ws://example.com/socket")) :-
        url::parse("ws://example.com/socket", Components),
        url::generate(Components, URL).

    test(roundtrip_2_12, true(URL == "wss://example.com/socket")) :-
        url::parse("wss://example.com/socket", Components),
        url::generate(Components, URL).

    test(roundtrip_2_13, true(URL == "ssh://user@host.example.com/")) :-
        url::parse("ssh://user@host.example.com/", Components),
        url::generate(Components, URL).

    test(roundtrip_2_14, true(URL == "git://github.com/user/repo.git")) :-
        url::parse("git://github.com/user/repo.git", Components),
        url::generate(Components, URL).

    test(roundtrip_2_15, true(URL == "ldap://ldap.example.com/dc=example,dc=com")) :-
        url::parse("ldap://ldap.example.com/dc=example,dc=com", Components),
        url::generate(Components, URL).
