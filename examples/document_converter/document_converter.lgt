%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(document).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-09-18,
		comment is 'Document to text conversion example using the Apache Tika Java library.'
	]).

	:- public(convert/2).
	:- mode(convert(+atom, +atom), one).
	:- info(convert/2, [
		comment is 'Converts a document given its path to a text file using UTF-8 encoding.',
		argnames is ['Document', 'Text']
	]).

	:- public(contents/2).
	:- mode(contents(+atom, -atom), one).
	:- info(contents/2, [
		comment is 'Returns the text contents of a document given its path.',
		argnames is ['Document', 'Contents']
	]).

	:- uses(type, [check/3]).

	convert(Document, Text) :-
		% type check arguments to minimize the possible exceptions in the Java side
		context(Context),
		check(file, Document, Context),
		check(atom, Text, Context),
		convert_file(Document, Text).

	convert_file(Document, Text) :-
		catch(
			convert_file_java(Document, Text),
			error(_, JavaException),
			resource_error(JavaException)
		).

	convert_file_java(Document, Text) :-
		% parse method arguments
		java('org.apache.tika.parser.AutoDetectParser')::new(AutoDetectParser),
		java('java.io.FileOutputStream')::new([Text], FileOutputStream),
		java('java.nio.charset.Charset', Charset)::forName('UTF8'),
		java('java.io.OutputStreamWriter')::new([FileOutputStream, Charset], OutputStreamWriter),
		java('org.apache.tika.sax.BodyContentHandler')::new([OutputStreamWriter], BodyContentHandler),
		java('org.apache.tika.metadata.Metadata')::new(Metadata),
		java('java.io.FileInputStream')::new([Document], FileInputStream),
		java('org.apache.tika.parser.ParseContext')::new(ParseContext),
		% file parsing
		java(AutoDetectParser)::parse(FileInputStream, BodyContentHandler, Metadata, ParseContext),
		java(FileInputStream)::close,
		java(FileOutputStream)::close.

	contents(Document, Contents) :-
		% type check argument to minimize the possible exceptions in the Java side
		context(Context),
		check(file, Document, Context),
		check(var, Contents, Context),
		contents_file(Document, Contents).

	contents_file(Document, Text) :-
		catch(
			contents_file_java(Document, Text),
			error(_, JavaException),
			resource_error(JavaException)
		).

	contents_file_java(Document, Contents) :-
		% parse method arguments
		java('org.apache.tika.parser.AutoDetectParser')::new(AutoDetectParser),
		java('org.apache.tika.sax.BodyContentHandler')::new([-1], BodyContentHandler),
		java('org.apache.tika.metadata.Metadata')::new(Metadata),
		java('java.io.FileInputStream')::new([Document], FileInputStream),
		java('org.apache.tika.parser.ParseContext')::new(ParseContext),
		% file parsing
		java(AutoDetectParser)::parse(FileInputStream, BodyContentHandler, Metadata, ParseContext),
		java(BodyContentHandler, Contents)::toString.

:- end_object.
