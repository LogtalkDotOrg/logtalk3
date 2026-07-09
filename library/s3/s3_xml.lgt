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


:- object(s3_xml).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Adapter layer mapping S3 XML responses to stable library terms.'
	]).

	:- public(list_buckets_response/2).
	:- mode(list_buckets_response(+compound, -compound), one_or_error).
	:- info(list_buckets_response/2, [
		comment is 'Parses a ListBuckets XML response body.',
		argnames is ['Body', 'Buckets'],
		exceptions is [
			'``Body`` is not a supported XML response body term' - domain_error(s3_xml_body, 'Body'),
			'The XML document is malformed or missing required elements' - domain_error(s3_xml_document, 'Codes')
		]
	]).

	:- public(list_objects_v2_response/2).
	:- mode(list_objects_v2_response(+compound, -compound), one_or_error).
	:- info(list_objects_v2_response/2, [
		comment is 'Parses a ListObjectsV2 XML response body.',
		argnames is ['Body', 'Listing'],
		exceptions is [
			'``Body`` is not a supported XML response body term' - domain_error(s3_xml_body, 'Body'),
			'The XML document is malformed or missing required elements' - domain_error(s3_xml_document, 'Codes')
		]
	]).

	:- public(copy_object_response/2).
	:- mode(copy_object_response(+compound, -compound), one_or_error).
	:- info(copy_object_response/2, [
		comment is 'Parses a CopyObject XML response body.',
		argnames is ['Body', 'Result'],
		exceptions is [
			'``Body`` is not a supported XML response body term' - domain_error(s3_xml_body, 'Body'),
			'The XML document is malformed or missing required elements' - domain_error(s3_xml_document, 'Codes')
		]
	]).

	:- public(create_multipart_upload_response/2).
	:- mode(create_multipart_upload_response(+compound, -compound), one_or_error).
	:- info(create_multipart_upload_response/2, [
		comment is 'Parses a CreateMultipartUpload XML response body.',
		argnames is ['Body', 'Result'],
		exceptions is [
			'``Body`` is not a supported XML response body term' - domain_error(s3_xml_body, 'Body'),
			'The XML document is malformed or missing required elements' - domain_error(s3_xml_document, 'Codes')
		]
	]).

	:- public(error_response/2).
	:- mode(error_response(+compound, -compound), zero_or_one_or_error).
	:- info(error_response/2, [
		comment is 'Parses an S3 XML error response body.',
		argnames is ['Body', 'Error'],
		exceptions is [
			'``Body`` is not a supported XML response body term' - domain_error(s3_xml_body, 'Body'),
			'The XML document is malformed or missing required elements' - domain_error(s3_xml_document, 'Codes')
		]
	]).

	:- uses(list, [
		append/2, append/3, valid/1 as is_list/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	list_buckets_response(Body, buckets(Buckets)) :-
		document_content(Body, Content),
		root_element(Content, 'ListAllMyBucketsResult', RootContent),
		(	child_element(RootContent, 'Buckets', BucketsContent) ->
			bucket_entries(BucketsContent, Buckets)
		;	Buckets = []
		).

	list_objects_v2_response(Body, objects(Properties, Entries, Prefixes)) :-
		document_content(Body, Content),
		root_element(Content, 'ListBucketResult', RootContent),
		list_bucket_properties(RootContent, Properties),
		object_entries(RootContent, Entries),
		common_prefixes(RootContent, Prefixes).

	copy_object_response(Body, copy_result(ETag, Properties)) :-
		document_content(Body, Content),
		root_element(Content, 'CopyObjectResult', RootContent),
		required_child_text(RootContent, 'ETag', ETag),
		optional_child_property(RootContent, 'LastModified', last_modified, LastModifiedProperties),
		Properties = LastModifiedProperties.

	create_multipart_upload_response(Body, multipart_upload(Bucket, Key, UploadId, [])) :-
		document_content(Body, Content),
		root_element(Content, 'InitiateMultipartUploadResult', RootContent),
		required_child_text(RootContent, 'Bucket', Bucket),
		required_child_text(RootContent, 'Key', Key),
		required_child_text(RootContent, 'UploadId', UploadId).

	error_response(Body, s3_error(Code, Message, Properties)) :-
		document_content(Body, Content),
		root_element(Content, 'Error', ErrorContent),
		required_child_text(ErrorContent, 'Code', Code),
		required_child_text(ErrorContent, 'Message', Message),
		error_properties(ErrorContent, Properties).

	document_content(content(_MediaType, text(Text)), Content) :-
		!,
		text_codes(Text, Codes),
		parse_document(Codes, Content).
	document_content(content(_MediaType, binary(Bytes)), Content) :-
		!,
		parse_document(Bytes, Content).
	document_content(Body, _Content) :-
		domain_error(s3_xml_body, Body).

	parse_document(Codes, Content) :-
		xml::parse(Codes, xml(_Attributes, Content)),
		!.
	parse_document(Codes, _Content) :-
		xml::parse(Codes, malformed(_Attributes, _Terms)),
		!,
		domain_error(s3_xml_malformed, Codes).
	parse_document(Codes, _Content) :-
		domain_error(s3_xml_document, Codes).

	root_element(Content, Name, ElementContent) :-
		content_element(Content, Name, _Attributes, ElementContent),
		!.
	root_element(_Content, Name, _ElementContent) :-
		existence_error(s3_xml_element, Name).

	bucket_entries(Content, Buckets) :-
		findall(
			bucket(Name, CreationDate, Properties),
			(
				content_element(Content, 'Bucket', _BucketAttributes, BucketContent),
				required_child_text(BucketContent, 'Name', Name),
				required_child_text(BucketContent, 'CreationDate', CreationDate),
				bucket_properties(BucketContent, Properties)
			),
			Buckets
		).

	bucket_properties(Content, Properties) :-
		optional_child_property(Content, 'Region', region, RegionProperties),
		optional_child_property(Content, 'BucketArn', bucket_arn, ARNProperties),
		append(RegionProperties, ARNProperties, Properties).

	list_bucket_properties(Content, Properties) :-
		optional_child_property(Content, 'Name', name, NameProperties),
		optional_child_property(Content, 'Prefix', prefix, PrefixProperties),
		optional_child_property(Content, 'Delimiter', delimiter, DelimiterProperties),
		optional_child_property(Content, 'EncodingType', encoding_type, EncodingTypeProperties),
		optional_child_property(Content, 'ContinuationToken', continuation_token, ContinuationTokenProperties),
		optional_child_property(Content, 'NextContinuationToken', next_continuation_token, NextContinuationTokenProperties),
		optional_child_property(Content, 'StartAfter', start_after, StartAfterProperties),
		optional_child_integer_property(Content, 'KeyCount', key_count, KeyCountProperties),
		optional_child_integer_property(Content, 'MaxKeys', max_keys, MaxKeysProperties),
		optional_child_boolean_property(Content, 'IsTruncated', is_truncated, IsTruncatedProperties),
		append([
			NameProperties,
			PrefixProperties,
			DelimiterProperties,
			EncodingTypeProperties,
			ContinuationTokenProperties,
			NextContinuationTokenProperties,
			StartAfterProperties,
			KeyCountProperties,
			MaxKeysProperties,
			IsTruncatedProperties
		], Properties).

	object_entries(Content, Entries) :-
		findall(
			object(Key, Size, ETag, LastModified, Properties),
			(
				content_element(Content, 'Contents', _ObjectAttributes, ObjectContent),
				required_child_text(ObjectContent, 'Key', Key),
				required_child_integer(ObjectContent, 'Size', Size),
				required_child_text(ObjectContent, 'ETag', ETag),
				required_child_text(ObjectContent, 'LastModified', LastModified),
				object_properties(ObjectContent, Properties)
			),
			Entries
		).

	object_properties(Content, Properties) :-
		optional_child_property(Content, 'StorageClass', storage_class, StorageClassProperties),
		optional_owner_property(Content, OwnerProperties),
		append(StorageClassProperties, OwnerProperties, Properties).

	optional_owner_property(Content, [owner(OwnerProperties)]) :-
		child_element(Content, 'Owner', OwnerContent),
		!,
		optional_child_property(OwnerContent, 'ID', id, IDProperties),
		optional_child_property(OwnerContent, 'DisplayName', display_name, NameProperties),
		append(IDProperties, NameProperties, OwnerProperties).
	optional_owner_property(_Content, []).

	common_prefixes(Content, Prefixes) :-
		findall(
			prefix(Prefix),
			(
				content_element(Content, 'CommonPrefixes', _Attributes, PrefixContent),
				required_child_text(PrefixContent, 'Prefix', Prefix)
			),
			Prefixes
		).

	error_properties(Content, Properties) :-
		optional_child_property(Content, 'Resource', resource, ResourceProperties),
		optional_child_property(Content, 'RequestId', request_id, RequestIdProperties),
		optional_child_property(Content, 'HostId', host_id, HostIdProperties),
		append([ResourceProperties, RequestIdProperties, HostIdProperties], Properties).

	child_element(Content, Name, ElementContent) :-
		content_element(Content, Name, _Attributes, ElementContent).

	content_element([Node| _], Name, Attributes, ElementContent) :-
		element_node(Node, Name, Attributes, ElementContent).
	content_element([_| Content], Name, Attributes, ElementContent) :-
		content_element(Content, Name, Attributes, ElementContent).

	element_node(element(Name, Attributes, ElementContent), Name, Attributes, ElementContent).
	element_node(namespace(_URI, _Prefix, Element), Name, Attributes, ElementContent) :-
		element_node(Element, Name, Attributes, ElementContent).

	required_child_text(Content, Name, Text) :-
		(	child_text(Content, Name, Text) ->
			true
		;	existence_error(s3_xml_element, Name)
		).

	required_child_integer(Content, Name, Integer) :-
		required_child_text(Content, Name, Text),
		atom_codes(Text, Codes),
		number_codes(Integer, Codes).

	child_text(Content, Name, Text) :-
		child_element(Content, Name, ElementContent),
		element_text(ElementContent, Text).

	element_text(Content, Text) :-
		findall(Fragment, text_fragment(Content, Fragment), Fragments),
		Fragments \== [],
		atomic_list_concat(Fragments, Text).

	text_fragment([pcdata(Codes)| _], Fragment) :-
		atom_codes(Fragment, Codes).
	text_fragment([_| Content], Fragment) :-
		text_fragment(Content, Fragment).

	optional_child_property(Content, Name, Functor, [Property]) :-
		child_text(Content, Name, Text),
		!,
		Property =.. [Functor, Text].
	optional_child_property(_Content, _Name, _Functor, []).

	optional_child_integer_property(Content, Name, Functor, [Property]) :-
		child_text(Content, Name, Text),
		!,
		atom_codes(Text, Codes),
		number_codes(Integer, Codes),
		Property =.. [Functor, Integer].
	optional_child_integer_property(_Content, _Name, _Functor, []).

	optional_child_boolean_property(Content, Name, Functor, [Property]) :-
		child_text(Content, Name, Text),
		!,
		boolean_atom(Text, Boolean),
		Property =.. [Functor, Boolean].
	optional_child_boolean_property(_Content, _Name, _Functor, []).

	boolean_atom('true', true).
	boolean_atom('false', false).
	boolean_atom(Text, _Boolean) :-
		domain_error(s3_xml_boolean, Text).

	text_codes(Text, Codes) :-
		atom(Text),
		!,
		atom_codes(Text, Codes).
	text_codes(Codes, Codes) :-
		is_list(Codes),
		!.
	text_codes(Text, _Codes) :-
		type_error(text, Text).

:- end_object.
