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

    test(valid_1_01, deterministic) :-
        url::valid("http://example.com").

    test(valid_1_02, deterministic) :-
        url::valid("https://example.com/path/to/resource").

    test(valid_1_03, deterministic) :-
        url::valid("http://example.com/path/to/file.html").

    test(valid_1_04, deterministic) :-
        url::valid("http://example.com/path/to/file.html?param1=value1&param2=value2").

    test(valid_1_05, deterministic) :-
        url::valid("http://example.com/path/to/file.html?param=value#section").

    test(valid_1_06, deterministic) :-
        url::valid("http://192.168.1.1/path/to/resource").

    test(valid_1_07, deterministic) :-
        url::valid("ftp://ftp.example.com/pub/file.zip").

    test(valid_1_08, deterministic) :-
        url::valid("rtsp://media.example.com/stream").

    test(valid_1_09, deterministic) :-
        url::valid("file:///home/user/document.txt").

    test(valid_1_10, deterministic) :-
        url::valid("mailto:user@example.com").

    test(valid_1_11, false) :-
        url::valid("invalid url").

    test(valid_1_12, false) :-
        url::valid("http:example.com").

    test(valid_1_19, false) :-
        url::valid("://broken").

    test(valid_1_20, false) :-
        url::valid("users:80").

    test(valid_1_21, false) :-
        url::valid("").

    test(valid_1_22, deterministic) :-
        url::valid("file://server/share/app/file.txt").

    test(valid_1_22a, false) :-
        url::valid("file://user@server/share/app/file.txt").

    test(valid_1_22b, false) :-
        url::valid("file://server:80/share/app/file.txt").

    test(valid_1_23, deterministic) :-
        url::valid("http://user.name@example.com/path").

    test(valid_1_24, deterministic) :-
        url::valid("http://user!info@example.com/path").

    test(valid_1_25, deterministic) :-
        url::valid("http://[2001:db8::1]/path").

    % reference_kind/2 tests

    test(reference_kind_2_01, deterministic(Kind == url)) :-
        url::reference_kind("http://example.com", Kind).

    test(reference_kind_2_02, deterministic(Kind == network_path)) :-
        url::reference_kind("//example.com/v1", Kind).

    test(reference_kind_2_03, deterministic(Kind == absolute_path)) :-
        url::reference_kind("/v1", Kind).

    test(reference_kind_2_04, deterministic(Kind == relative_path)) :-
        url::reference_kind("users/42", Kind).

    test(reference_kind_2_05, deterministic(Kind == query)) :-
        url::reference_kind("?page=2", Kind).

    test(reference_kind_2_06, deterministic(Kind == fragment)) :-
        url::reference_kind("#users", Kind).

    % parse/3 tests

    test(parse_3_01, deterministic) :-
        url::parse("/v1", Components, Kind),
        Components == [path("/v1")],
        Kind == absolute_path.

    test(parse_3_02, deterministic) :-
        url::parse("../users/42?verbose=true#details", Components, Kind),
        Components == [path("../users/42"), query("verbose=true"), fragment("details")],
        Kind == relative_path.

    test(parse_3_03, deterministic) :-
        url::parse("users/42?verbose=true#details", Components, Kind),
        Components == [path("users/42"), query("verbose=true"), fragment("details")],
        Kind == relative_path.

    test(parse_3_04, deterministic) :-
        url::parse("//example.com/v1", Components, Kind),
        Components == [authority("example.com"), path("/v1")],
        Kind == network_path.

    test(parse_3_05, deterministic) :-
        url::parse("?page=2", Components, Kind),
        Components == [query("page=2")],
        Kind == query.

    test(parse_3_06, deterministic) :-
        url::parse("#users", Components, Kind),
        Components == [fragment("users")],
        Kind == fragment.

    test(parse_3_07, deterministic) :-
        url::parse("//example.com", Components, Kind),
        Components == [authority("example.com")],
        Kind == network_path.

    % equivalent/2 tests

    test(equivalent_2_01, deterministic) :-
        url::equivalent("HTTP://example.com/path/../resource", "http://example.com/resource").

    test(equivalent_2_02, deterministic) :-
        url::equivalent("#fragment with spaces[2]", "#fragment%20with%20spaces%5B2%5D").

    test(equivalent_2_03, false) :-
        url::equivalent("http://example.com/a", "http://example.com/b").

    test(equivalent_2_04, deterministic) :-
        url::equivalent("http://EXAMPLE.COM", "http://example.com").

    test(equivalent_2_05, deterministic) :-
        url::equivalent("http://example.com/%7Euser", "http://example.com/~user").

    % parse/2 tests

    test(parse_2_01, deterministic(Components == [scheme("http"), authority("example.com"), path(""), query(""), fragment("")])) :-
        url::parse("http://example.com", Components).

    test(parse_2_02, deterministic(Components == [scheme("https"), authority("example.com"), path("/path/to"), query(""), fragment("")])) :-
        url::parse("https://example.com/path/to", Components).

    test(parse_2_03, deterministic(Components == [scheme("http"), authority("example.com"), path("/path/to/file.html"), query(""), fragment("")])) :-
        url::parse("http://example.com/path/to/file.html", Components).

    test(parse_2_04, deterministic(Components == [scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param1=value1&param2=value2"), fragment("")])) :-
        url::parse("http://example.com/path/to/file.html?param1=value1&param2=value2", Components).

    test(parse_2_05, deterministic(Components == [scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param=value"), fragment("section")])) :-
        url::parse("http://example.com/path/to/file.html?param=value#section", Components).

    test(parse_2_06, deterministic(Components == [scheme("http"), authority("192.168.1.1"), path("/path/to/resource"), query(""), fragment("")])) :-
        url::parse("http://192.168.1.1/path/to/resource", Components).

    test(parse_2_07, deterministic(Components == [scheme("ftp"), authority("ftp.example.com"), path("/pub/file.zip")])) :-
        url::parse("ftp://ftp.example.com/pub/file.zip", Components).

    test(parse_2_08, deterministic(Components == [scheme("rtsp"), authority("media.example.com"), path("/stream")])) :-
        url::parse("rtsp://media.example.com/stream", Components).

    % File URI tests
    test(parse_2_09, deterministic) :-
        url::parse("file:///home/user/document.txt", _Components).

    % file_path_components/2 tests
    test(file_path_components_2_01, deterministic(Components == [authority(""), path("/home/user/document.txt")])) :-
        url::file_path_components("/home/user/document.txt", Components).

    test(file_path_components_2_02, deterministic(Components == [authority(""), path("/C:/Temp/app/file.txt")])) :-
        url::file_path_components("C:\\Temp\\app\\file.txt", Components).

    test(file_path_components_2_03, deterministic(Components == [authority("server"), path("/share/app/file.txt")])) :-
        url::file_path_components("\\\\server\\share\\app\\file.txt", Components).

    test(file_path_components_2_04, deterministic(Components == [authority(""), path("/C:/Temp/app/")])) :-
        url::file_path_components("C:\\Temp\\app\\", Components).

    test(file_path_components_2_05, deterministic(Components == [authority("server"), path("/share/app/")])) :-
        url::file_path_components("\\\\server\\share\\app\\", Components).

    test(file_path_components_2_06, deterministic(Components == [authority(""), path("/c:/Temp/app/file.txt")])) :-
        url::file_path_components("c:\\Temp\\app\\file.txt", Components).

    % Mailto URI tests
    test(parse_2_10, deterministic) :-
        url::parse("mailto:user@example.com", _Components).

    % normalize/2 tests

    test(normalize_2_01, deterministic(NormalizedURL == "http://example.com")) :-
        url::normalize("HTTP://example.com", NormalizedURL).

    test(normalize_2_02, deterministic(NormalizedURL == "http://example.com/path/")) :-
        url::normalize("http://example.com/path/", NormalizedURL).

    test(normalize_2_03, deterministic(NormalizedURL == "http://example.com/path/to/resource")) :-
        url::normalize("http://example.com/path/to/resource", NormalizedURL).

    test(normalize_2_04, deterministic(NormalizedURL == "http://example.com/path/to/resource")) :-
        url::normalize("http://example.com/path/to/../to/resource", NormalizedURL).

    test(normalize_2_05, deterministic(NormalizedURL == "http://example.com/resource")) :-
        url::normalize("http://example.com/path/../resource", NormalizedURL).

    test(normalize_2_06, deterministic(NormalizedURL == "http://example.com/path/to/resource?query=value#fragment")) :-
        url::normalize("http://example.com/path/to/resource?query=value#fragment", NormalizedURL).

    test(normalize_2_07, deterministic(NormalizedURL == "http://example.com/path%20with%20spaces")) :-
        url::normalize("http://example.com/path with spaces", NormalizedURL).

    test(normalize_2_08, deterministic(NormalizedURL == "http://example.com/path%5Bdraft%5D")) :-
        url::normalize("http://example.com/path[draft]", NormalizedURL).

    test(normalize_2_09, deterministic(NormalizedURL == "http://example.com/path/to/resource?query=value%20with%20spaces%7Cpipe")) :-
        url::normalize("http://example.com/path/to/resource?query=value with spaces|pipe", NormalizedURL).

    test(normalize_2_10, deterministic(NormalizedURL == "http://example.com/path/to/resource#fragment%20with%20spaces%5B2%5D")) :-
        url::normalize("http://example.com/path/to/resource#fragment with spaces[2]", NormalizedURL).

    test(normalize_2_11, deterministic(NormalizedURL == "http://example.com/path%20with%2Fencoded%25zz")) :-
        url::normalize("http://example.com/path%20with%2fencoded%zz", NormalizedURL).

    test(normalize_2_12, deterministic(NormalizedURL == "mailto:user%2Fname@example.com")) :-
        url::normalize("MAILTO:user%2fname@example.com", NormalizedURL).

    test(normalize_2_13, deterministic(NormalizedURL == "tel:+1%20816%20555%201212")) :-
        url::normalize("TEL:+1 816 555 1212", NormalizedURL).

    test(normalize_2_14, deterministic(NormalizedURL == "urn:example:hello%2Fworld")) :-
        url::normalize("URN:example:hello%2fworld", NormalizedURL).

    test(normalize_2_15, deterministic(NormalizedURL == "/resource")) :-
        url::normalize("/path/../resource", NormalizedURL).

    test(normalize_2_16, deterministic(NormalizedURL == "../users/42")) :-
        url::normalize("../users/./42", NormalizedURL).

    test(normalize_2_17, deterministic(NormalizedURL == "//example.com/v1")) :-
        url::normalize("//example.com/path/../v1", NormalizedURL).

    test(normalize_2_18, deterministic(NormalizedURL == "?query=value%20with%20spaces%7Cpipe")) :-
        url::normalize("?query=value with spaces|pipe", NormalizedURL).

    test(normalize_2_19, deterministic(NormalizedURL == "#fragment%20with%20spaces%5B2%5D")) :-
        url::normalize("#fragment with spaces[2]", NormalizedURL).

    test(normalize_2_20, deterministic(NormalizedURL == ("/"))) :-
        url::normalize("/", NormalizedURL).

    test(normalize_2_21, deterministic(NormalizedURL == "tel:+1%20816.555.1212")) :-
        url::normalize("TEL:+1 816.555.1212", NormalizedURL).

    test(normalize_2_22, deterministic(NormalizedURL == "http://example.com")) :-
        url::normalize("http://EXAMPLE.COM", NormalizedURL).

    test(normalize_2_23, deterministic(NormalizedURL == "http://example.com/~user")) :-
        url::normalize("http://example.com/%7Euser", NormalizedURL).

    test(normalize_2_24, deterministic(NormalizedURL == "/b")) :-
        url::normalize("/a/../../b", NormalizedURL).

    test(normalize_2_25, deterministic(NormalizedURL == "//example.com/b")) :-
        url::normalize("//example.com/a/../../b", NormalizedURL).

    test(normalize_2_26, deterministic(NormalizedURL == "../../b")) :-
        url::normalize("../a/../../b", NormalizedURL).

    % Additional tests for edge cases

    test(url_edge_case_01, deterministic) :-
        url::valid("http://example.com:8080/path").

    test(url_edge_case_02, deterministic) :-
        url::valid("http://user:password@example.com/path").

    test(url_edge_case_03, deterministic) :-
        url::valid("http://example.com/path%20with%20spaces").

    test(url_edge_case_04, deterministic) :-
        url::valid("http://example.com/path?query=value%20with%20spaces").

    test(url_edge_case_05, deterministic) :-
        url::valid("http://example.com/path?query=value#fragment%20with%20spaces").

    test(url_edge_case_06, deterministic) :-
        url::valid("mailto:user+tag@example.com").

    test(url_edge_case_07, deterministic) :-
        url::valid("file:///c:/Windows/System32/").

    test(url_edge_case_08, deterministic) :-
        url::valid("https://example.com/").

    test(url_edge_case_09, deterministic) :-
        url::valid("https://example.com/?").

    test(url_edge_case_10, deterministic) :-
        url::valid("https://example.com/#").

    % New URL schemes tests

    test(url_scheme_ldap_01, deterministic) :-
        url::valid("ldap://ldap.example.com/dc=example,dc=com").

    test(url_scheme_ldap_02, deterministic) :-
        url::valid("ldap://ldap.example.com/dc=example,dc=com?cn").

    test(url_scheme_news_01, deterministic) :-
        url::valid("news:comp.infosystems.www.servers.unix").

    test(url_scheme_news_02, deterministic) :-
        url::valid("news:example.message-id@example.com").

    test(url_scheme_tel_01, deterministic) :-
        url::valid("tel:+1-816-555-1212").

    test(url_scheme_tel_02, deterministic) :-
        url::valid("tel:+44(0)2012345678").

    test(url_scheme_tel_03, deterministic) :-
        url::valid("tel:+1 816.555.1212").

    test(url_scheme_telnet_01, deterministic) :-
        url::valid("telnet://192.0.2.16:80/").

    test(url_scheme_telnet_02, deterministic) :-
        url::valid("telnet://example.com/").

    test(url_scheme_urn_01, deterministic) :-
        url::valid("urn:oasis:names:specification:docbook:dtd:xml:4.1.2").

    test(url_scheme_urn_02, deterministic) :-
        url::valid("urn:isbn:0451450523").

    % RFC3986 edge cases

    test(rfc3986_empty_authority, deterministic) :-
        url::valid("file:///path/to/file").

    test(rfc3986_port_number, deterministic) :-
        url::valid("http://example.com:8080/path").

    test(rfc3986_empty_path, deterministic) :-
        url::valid("http://example.com").

    test(rfc3986_query_only, deterministic) :-
        url::valid("http://example.com?query").

    test(rfc3986_fragment_only, deterministic) :-
        url::valid("http://example.com#fragment").

    test(rfc3986_unreserved_chars, deterministic) :-
        url::valid("http://example.com/path-with_under.tilde~").

    test(rfc3986_sub_delims, deterministic) :-
        url::valid("http://example.com/path!$&'()*+,;=test").

    test(rfc3986_case_insensitive_scheme_01, deterministic) :-
        url::valid("HTTP://example.com/").

    test(rfc3986_case_insensitive_scheme_02, deterministic) :-
        url::valid("HtTp://example.com/").

    test(rfc3986_case_insensitive_scheme_03, deterministic) :-
        url::valid("HTTPS://example.com/").

    test(rfc3986_case_insensitive_scheme_04, deterministic) :-
        url::valid("FTP://example.com/").

    test(rfc3986_case_insensitive_scheme_05, deterministic) :-
        url::valid("MAILTO:user@example.com").

    % Parse URL tests for new schemes

    test(parse_ldap_01, deterministic(Components == [scheme("ldap"), authority("ldap.example.com"), path("/dc=example,dc=com"), query("")])) :-
        url::parse("ldap://ldap.example.com/dc=example,dc=com", Components).

    test(parse_news_01, deterministic(Components == [scheme("news"), identifier("comp.infosystems.www.servers.unix")])) :-
        url::parse("news:comp.infosystems.www.servers.unix", Components).

    test(parse_tel_01, deterministic(Components == [scheme("tel"), number("+1-816-555-1212")])) :-
        url::parse("tel:+1-816-555-1212", Components).

    test(parse_telnet_01, deterministic(Components == [scheme("telnet"), authority("192.0.2.16:80"), path("/")])) :-
        url::parse("telnet://192.0.2.16:80/", Components).

    test(parse_urn_01, deterministic(Components == [scheme("urn"), identifier("oasis:names:specification:docbook:dtd:xml:4.1.2")])) :-
        url::parse("urn:oasis:names:specification:docbook:dtd:xml:4.1.2", Components).

    % Additional URL schemes tests

    % FTPS (FTP Secure)
    test(url_scheme_ftps_01, deterministic) :-
        url::valid("ftps://ftp.example.com/pub/file.zip").

    test(parse_ftps_01, deterministic(Components == [scheme("ftps"), authority("ftp.example.com"), path("/pub/file.zip")])) :-
        url::parse("ftps://ftp.example.com/pub/file.zip", Components).

    % SFTP (SSH File Transfer Protocol)
    test(url_scheme_sftp_01, deterministic) :-
        url::valid("sftp://user@server.example.com/path/to/file").

    test(parse_sftp_01, deterministic(Components == [scheme("sftp"), authority("user@server.example.com"), path("/path/to/file")])) :-
        url::parse("sftp://user@server.example.com/path/to/file", Components).

    % SSH (Secure Shell)
    test(url_scheme_ssh_01, deterministic) :-
        url::valid("ssh://user@host.example.com/").

    test(parse_ssh_01, deterministic(Components == [scheme("ssh"), authority("user@host.example.com"), path("/")])) :-
        url::parse("ssh://user@host.example.com/", Components).

    % LDAPS (LDAP Secure)
    test(url_scheme_ldaps_01, deterministic) :-
        url::valid("ldaps://ldap.example.com/dc=example,dc=com").

    test(parse_ldaps_01, deterministic(Components == [scheme("ldaps"), authority("ldap.example.com"), path("/dc=example,dc=com"), query("")])) :-
        url::parse("ldaps://ldap.example.com/dc=example,dc=com", Components).

    % RTMP (Real-Time Messaging Protocol)
    test(url_scheme_rtmp_01, deterministic) :-
        url::valid("rtmp://media.example.com/live/stream").

    test(parse_rtmp_01, deterministic(Components == [scheme("rtmp"), authority("media.example.com"), path("/live/stream")])) :-
        url::parse("rtmp://media.example.com/live/stream", Components).

    % MMS (Microsoft Media Server)
    test(url_scheme_mms_01, deterministic) :-
        url::valid("mms://media.example.com/stream").

    test(parse_mms_01, deterministic(Components == [scheme("mms"), authority("media.example.com"), path("/stream")])) :-
        url::parse("mms://media.example.com/stream", Components).

    % JDBC (Java Database Connectivity)
    test(url_scheme_jdbc_01, deterministic) :-
        url::valid("jdbc://localhost:3306/database").

    test(parse_jdbc_01, deterministic(Components == [scheme("jdbc"), authority("localhost:3306"), path("/database")])) :-
        url::parse("jdbc://localhost:3306/database", Components).

    % MongoDB
    test(url_scheme_mongodb_01, deterministic) :-
        url::valid("mongodb://localhost:27017/mydb").

    test(parse_mongodb_01, deterministic(Components == [scheme("mongodb"), authority("localhost:27017"), path("/mydb")])) :-
        url::parse("mongodb://localhost:27017/mydb", Components).

    % MySQL
    test(url_scheme_mysql_01, deterministic) :-
        url::valid("mysql://user:password@localhost:3306/database").

    test(parse_mysql_01, deterministic(Components == [scheme("mysql"), authority("user:password@localhost:3306"), path("/database")])) :-
        url::parse("mysql://user:password@localhost:3306/database", Components).

    % PostgreSQL
    test(url_scheme_postgresql_01, deterministic) :-
        url::valid("postgresql://localhost:5432/mydb").

    test(parse_postgresql_01, deterministic(Components == [scheme("postgresql"), authority("localhost:5432"), path("/mydb")])) :-
        url::parse("postgresql://localhost:5432/mydb", Components).

    % WebSocket (ws)
    test(url_scheme_ws_01, deterministic) :-
        url::valid("ws://example.com/socket").

    test(parse_ws_01, deterministic(Components == [scheme("ws"), authority("example.com"), path("/socket"), query(""), fragment("")])) :-
        url::parse("ws://example.com/socket", Components).

    % WebSocket Secure (wss)
    test(url_scheme_wss_01, deterministic) :-
        url::valid("wss://example.com/socket").

    test(parse_wss_01, deterministic(Components == [scheme("wss"), authority("example.com"), path("/socket"), query(""), fragment("")])) :-
        url::parse("wss://example.com/socket", Components).

    % Gopher
    test(url_scheme_gopher_01, deterministic) :-
        url::valid("gopher://gopher.example.com/").

    test(parse_gopher_01, deterministic(Components == [scheme("gopher"), authority("gopher.example.com"), path("/")])) :-
        url::parse("gopher://gopher.example.com/", Components).

    % NNTP (Network News Transfer Protocol)
    test(url_scheme_nntp_01, deterministic) :-
        url::valid("nntp://news.example.com/").

    test(parse_nntp_01, deterministic(Components == [scheme("nntp"), authority("news.example.com"), path("/")])) :-
        url::parse("nntp://news.example.com/", Components).

    % Git
    test(url_scheme_git_01, deterministic) :-
        url::valid("git://github.com/user/repo.git").

    test(parse_git_01, deterministic(Components == [scheme("git"), authority("github.com"), path("/user/repo.git")])) :-
        url::parse("git://github.com/user/repo.git", Components).

    % generate/2 tests

    test(generate_2_01, deterministic(URL == "http://example.com")) :-
        url::generate([scheme("http"), authority("example.com"), path(""), query(""), fragment("")], URL).

    test(generate_2_02, deterministic(URL == "https://example.com/path/to")) :-
        url::generate([scheme("https"), authority("example.com"), path("/path/to"), query(""), fragment("")], URL).

    test(generate_2_03, deterministic(URL == "http://example.com/path/to/file.html")) :-
        url::generate([scheme("http"), authority("example.com"), path("/path/to/file.html"), query(""), fragment("")], URL).

    test(generate_2_04, deterministic(URL == "http://example.com/path/to/file.html?param1=value1&param2=value2")) :-
        url::generate([scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param1=value1&param2=value2"), fragment("")], URL).

    test(generate_2_05, deterministic(URL == "http://example.com/path/to/file.html?param=value#section")) :-
        url::generate([scheme("http"), authority("example.com"), path("/path/to/file.html"), query("param=value"), fragment("section")], URL).

    test(generate_2_06, deterministic(URL == "ftp://ftp.example.com/pub/file.zip")) :-
        url::generate([scheme("ftp"), authority("ftp.example.com"), path("/pub/file.zip")], URL).

    test(generate_2_07, deterministic(URL == "mailto:user@example.com")) :-
        url::generate([scheme("mailto"), address("user@example.com")], URL).

    test(generate_2_08, deterministic(URL == "news:comp.infosystems.www.servers.unix")) :-
        url::generate([scheme("news"), identifier("comp.infosystems.www.servers.unix")], URL).

    test(generate_2_09, deterministic(URL == "tel:+1-816-555-1212")) :-
        url::generate([scheme("tel"), number("+1-816-555-1212")], URL).

    test(generate_2_10, deterministic(URL == "urn:oasis:names:specification:docbook:dtd:xml:4.1.2")) :-
        url::generate([scheme("urn"), identifier("oasis:names:specification:docbook:dtd:xml:4.1.2")], URL).

    test(generate_2_11, deterministic(URL == "ws://example.com/socket")) :-
        url::generate([scheme("ws"), authority("example.com"), path("/socket"), query(""), fragment("")], URL).

    test(generate_2_12, deterministic(URL == "wss://example.com/socket")) :-
        url::generate([scheme("wss"), authority("example.com"), path("/socket"), query(""), fragment("")], URL).

    test(generate_2_13, deterministic(URL == "http://example.com/path%20with%20spaces")) :-
        url::generate([scheme("HTTP"), authority("example.com"), path("/path with spaces"), query(""), fragment("")], URL).

    test(generate_2_14, deterministic(URL == "http://example.com/resource?query=value%20with%20spaces%7Cpipe#fragment%20with%20spaces%5B2%5D")) :-
        url::generate([scheme("HTTP"), authority("example.com"), path("/path/../resource"), query("query=value with spaces|pipe"), fragment("fragment with spaces[2]")], URL).

    test(generate_2_15, deterministic(URL == "http://example.com/path%20with%2Fencoded%25zz")) :-
        url::generate([scheme("HTTP"), authority("example.com"), path("/path%20with%2fencoded%zz"), query(""), fragment("")], URL).

    test(generate_2_16, deterministic(URL == "mailto:user%20name@example.com")) :-
        url::generate([scheme("MAILTO"), address("user name@example.com")], URL).

    test(generate_2_17, deterministic(URL == "tel:+1%20816%20555%201212")) :-
        url::generate([scheme("TEL"), number("+1 816 555 1212")], URL).

    test(generate_2_18, deterministic(URL == "urn:example:hello%20world")) :-
        url::generate([scheme("URN"), identifier("example:hello world")], URL).

    test(generate_2_19, deterministic(URL == "/v1")) :-
        url::generate([path("/v1")], URL).

    test(generate_2_20, deterministic(URL == "../users/42?verbose=true#details")) :-
        url::generate([path("../users/42"), query("verbose=true"), fragment("details")], URL).

    test(generate_2_21, deterministic(URL == "//example.com/v1")) :-
        url::generate([authority("example.com"), path("/v1")], URL).

    test(generate_2_22, deterministic(URL == "?page=2")) :-
        url::generate([query("page=2")], URL).

    test(generate_2_23, deterministic(URL == "#users")) :-
        url::generate([fragment("users")], URL).

    test(generate_2_24, deterministic(URL == "//example.com")) :-
        url::generate([authority("example.com")], URL).

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

    test(roundtrip_2_16, true(URL == "/v1")) :-
        url::parse("/v1", Components, _Kind),
        url::generate(Components, URL).

    test(roundtrip_2_17, true(URL == "../users/42?verbose=true#details")) :-
        url::parse("../users/42?verbose=true#details", Components, _Kind),
        url::generate(Components, URL).

    test(roundtrip_2_18, true(URL == "//example.com/v1")) :-
        url::parse("//example.com/v1", Components, _Kind),
        url::generate(Components, URL).

    test(roundtrip_2_19, true(URL == "?page=2")) :-
        url::parse("?page=2", Components, _Kind),
        url::generate(Components, URL).

    test(roundtrip_2_20, true(URL == "#users")) :-
        url::parse("#users", Components, _Kind),
        url::generate(Components, URL).

    % resolve/3 tests

    test(resolve_3_01, deterministic(Resolved == "http://example.com/a/d")) :-
        url::resolve("http://example.com/a/b/c", "../d", Resolved).

    test(resolve_3_02, deterministic(Resolved == "http://example.com/v1")) :-
        url::resolve("http://example.com/a/b/c", "/v1", Resolved).

    test(resolve_3_03, deterministic(Resolved == "http://cdn.example.com/lib.js")) :-
        url::resolve("http://example.com/a/b", "//cdn.example.com/lib.js", Resolved).

    test(resolve_3_04, deterministic(Resolved == "http://example.com/a/b?page=2")) :-
        url::resolve("http://example.com/a/b?x=1", "?page=2", Resolved).

    test(resolve_3_05, deterministic(Resolved == "http://example.com/a/b?x=1#frag")) :-
        url::resolve("http://example.com/a/b?x=1", "#frag", Resolved).

    test(resolve_3_06, deterministic(Resolved == "https://other.example.com/x")) :-
        url::resolve("http://example.com/a/b", "https://other.example.com/x", Resolved).

    test(resolve_3_07, deterministic(Resolved == "http://example.com/a")) :-
        url::resolve("http://example.com", "a", Resolved).

    % relativize/3 tests

    test(relativize_3_01, deterministic(Relative == "../d")) :-
        url::relativize("http://example.com/a/b/c", "http://example.com/a/d", Relative).

    test(relativize_3_02, deterministic(Relative == "../../v1")) :-
        url::relativize("http://example.com/a/b/c", "http://example.com/v1", Relative).

    test(relativize_3_03, deterministic(Relative == "?page=2")) :-
        url::relativize("http://example.com/a/b?x=1", "http://example.com/a/b?page=2", Relative).

    test(relativize_3_04, deterministic(Relative == "#frag")) :-
        url::relativize("http://example.com/a/b?x=1#old", "http://example.com/a/b?x=1#frag", Relative).

    test(relativize_3_05, deterministic(Relative == "b")) :-
        url::relativize("http://example.com/a/b?x=1", "http://example.com/a/b", Relative).

    test(relativize_3_06, fail) :-
        url::relativize("http://example.com/a/b", "https://other.example.com/a/b", _).

    test(relativize_3_07, fail) :-
        url::relativize("http://example.com/a/b/", "http://example.com/a/b/", _).

    test(relativize_3_08, deterministic(Relative == "a")) :-
        url::relativize("http://example.com", "http://example.com/a", Relative).

    test(relativize_3_09, deterministic(Relative == "a/")) :-
        url::relativize("http://example.com/", "http://example.com/a/", Relative).

    test(relativize_3_10, deterministic(Relative == "c/")) :-
        url::relativize("http://example.com/a/b/", "http://example.com/a/b/c/", Relative).

    test(relativize_3_11, deterministic(Relative == "/c:d")) :-
        url::relativize("http://example.com/", "http://example.com/c:d", Relative).
