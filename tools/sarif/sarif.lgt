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


:- object(sarif).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-01,
		comment is 'Shared SARIF report generator for tools implementing the diagnostics protocol.'
	]).

	:- public(term/2).
	:- mode(term(+list(compound), -compound), one).
	:- info(term/2, [
		comment is 'Returns a SARIF 2.1.0 report term for the given list of diagnostics tool specifications. Each specification must be a ``tool_spec(Tool, Target, Options)`` term.',
		argnames is ['Specs', 'Term']
	]).

	:- public(term/4).
	:- mode(term(+object_identifier, +nonvar, -compound, +list(compound)), one).
	:- info(term/4, [
		comment is 'Returns a SARIF 2.1.0 report term for a diagnostics tool target using the given options.',
		argnames is ['Tool', 'Target', 'Term', 'Options']
	]).

	:- public(generate/2).
	:- mode(generate(+list(compound), ++compound), one).
	:- info(generate/2, [
		comment is 'Generates a SARIF 2.1.0 report for the given list of diagnostics tool specifications and sink accepted by the json library. Each specification must be a ``tool_spec(Tool, Target, Options)`` term.',
		argnames is ['Specs', 'Sink']
	]).

	:- public(generate/4).
	:- mode(generate(+object_identifier, +nonvar, ++compound, +list(compound)), one).
	:- info(generate/4, [
		comment is 'Generates a SARIF 2.1.0 report for a diagnostics tool target using the given options and sink accepted by the json library.',
		argnames is ['Tool', 'Target', 'Sink', 'Options']
	]).

	:- uses(git, [
		branch/2 as git_branch/2, commit_hash/2 as git_commit_hash/2
	]).

	:- uses(json(curly, dash, atom), [
		generate/2 as json_generate/2
	]).

	:- uses(list, [
		append/2, append/3, length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(os, [
		decompose_file_name/3, delete_file/1, internal_os_path/2, path_concat/3, pid/1, shell/1,
		temporary_directory/1
	]).

	:- uses(reader, [
		line_to_codes/2
	]).

	:- uses(term_io, [
		write_term_to_atom/3
	]).

	:- uses(url(atom), [
		file_path_components/2 as url_file_path_components/2,
		generate/2 as url_generate/2, normalize/2 as url_normalize/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- uses(uuid, [
		uuid_v4/1
	]).

	generate(Specs, Sink) :-
		term(Specs, Term),
		json_generate(Sink, Term).

	term(Specs, Term) :-
		sarif_terms(Specs, RawTerm),
		normalize_json_term(RawTerm, Term).

	generate(Tool, Target, Sink, UserOptions) :-
		term(Tool, Target, Term, UserOptions),
		json_generate(Sink, Term).

	term(Tool, Target, Term, UserOptions) :-
		sarif_terms([tool_spec(Tool, Target, UserOptions)], RawTerm),
		normalize_json_term(RawTerm, Term).

	sarif_terms(Specs, json(['$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-Runs])) :-
		sarif_runs(Specs, Runs).

	sarif_runs([], []).
	sarif_runs([Spec| Specs], [Run| Runs]) :-
		sarif_spec_run(Spec, Run),
		sarif_runs(Specs, Runs).

	sarif_spec_run(tool_spec(Tool, Target, UserOptions), Run) :-
		sarif_term(Tool, Target, Run, UserOptions).

	sarif_term(Tool, Target, Run, UserOptions) :-
		context(Context),
		check(object, Tool, Context),
		(   conforms_to_protocol(Tool, tool_diagnostics_protocol) ->
			true
		;   domain_error(diagnostics_tool, Tool)
		),
		Tool::diagnostics_tool(Id, Name, Version, InformationURI, Properties),
		ToolInfo = diagnostics_tool(Id, Name, Version, InformationURI, Properties),
		Tool::diagnostic_rules(Rules),
		Tool::diagnostics(Target, Diagnostics, UserOptions),
		Tool::diagnostics_preflight(Target, Preflight, UserOptions),
		normalization_context(Target, Diagnostics, Properties, NormalizationContext),
		uuid_v4(RunGUID),
		tool_driver(ToolInfo, Rules, Driver),
		run_properties(Diagnostics, ToolInfo, NormalizationContext, RunProperties),
		sarif_results(Diagnostics, Rules, ToolInfo, NormalizationContext, Results),
		sarif_notifications(Preflight, NormalizationContext, Notifications),
		sarif_run(ToolInfo, Target, RunGUID, Driver, Diagnostics, NormalizationContext, RunProperties, Notifications, Results, Run).

	tool_driver(diagnostics_tool(_Id, Name, Version, InformationURI, Properties), Rules, json(Pairs)) :-
		rules_json(Rules, RulesJSON),
		BasePairs = [name-Name, informationUri-InformationURI, version-Version],
		(	member(guid(Guid), Properties) ->
			append(BasePairs, [guid-Guid, rules-RulesJSON], Pairs)
		;	append(BasePairs, [rules-RulesJSON], Pairs)
		).

	rules_json([], []).
	rules_json([Rule| Rules], [RuleJSON| RulesJSON]) :-
		rule_json(Rule, RuleJSON),
		rules_json(Rules, RulesJSON).

	rule_json(diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties), json(Pairs)) :-
		DefaultConfiguration = json([level-DefaultSeverity]),
		(	member(guid(Guid), Properties) ->
			Pairs = [
				id-RuleId,
				guid-Guid,
				name-RuleId,
				shortDescription-json([text-ShortDescription]),
				fullDescription-json([text-FullDescription]),
				defaultConfiguration-DefaultConfiguration
			]
		;	Pairs = [
				id-RuleId,
				name-RuleId,
				shortDescription-json([text-ShortDescription]),
				fullDescription-json([text-FullDescription]),
				defaultConfiguration-DefaultConfiguration
			]
		).

	sarif_run(ToolInfo, Target, RunGUID, Driver, Diagnostics, NormalizationContext, RunProperties, Notifications, Results, json(Pairs)) :-
		ToolInfo = diagnostics_tool(Id, _Name, _Version, _InformationURI, Properties),
		automation_id(Target, Id, Properties, AutomationId),
		original_uri_base_ids_pairs(NormalizationContext, BaseIdPairs),
		invocations_pairs(Properties, Notifications, InvocationPairs),
		version_control_pairs(Properties, Diagnostics, VersionControlPairs),
		BasePairs = [tool-json([driver-Driver]), automationDetails-json([id-AutomationId, guid-RunGUID])],
		append([BasePairs, BaseIdPairs, InvocationPairs, VersionControlPairs, [properties-RunProperties, results-Results]], Pairs).

	automation_id(Target, ToolId, Properties, AutomationId) :-
		(	member(automation_id(target), Properties) ->
			target_id(Target, AutomationId)
		;	AutomationId = ToolId
		).

	original_uri_base_ids_pairs(repository(RepositoryRoot, _Branch, _CommitHash), [originalUriBaseIds-json(['REPO_ROOT'-json([uri-RepositoryRootURI])])]) :-
		sarif_directory_uri(RepositoryRoot, RepositoryRootURI),
		!.
	original_uri_base_ids_pairs(application(ApplicationRoot), [originalUriBaseIds-json(['APP_ROOT'-json([uri-ApplicationRootURI])])]) :-
		sarif_directory_uri(ApplicationRoot, ApplicationRootURI),
		!.
	original_uri_base_ids_pairs(none, []).

	target_id(all, all) :-
		!.
	target_id(Target, AutomationId) :-
		to_atom(Target, AutomationId).

	invocations_pairs(Properties, Notifications, [invocations-[Invocation]]) :-
		memberchk(include_invocations(true), Properties),
		invocation_json(Notifications, Invocation),
		!.
	invocations_pairs(_Properties, _Notifications, []).

	invocation_json([], json([executionSuccessful- @true])).
	invocation_json(Notifications, json([executionSuccessful- @true, toolExecutionNotifications-Notifications])).

	version_control_pairs(Properties, Diagnostics, [versionControlProvenance-VersionControl]) :-
		memberchk(include_version_control_provenance(true), Properties),
		sarif_version_control_provenance(Diagnostics, VersionControl),
		VersionControl \== [],
		!.
	version_control_pairs(_Properties, _Diagnostics, []).

	run_properties(Diagnostics, diagnostics_tool(_Id, _Name, _Version, _InformationURI, Properties), NormalizationContext, json(Pairs)) :-
		length(Diagnostics, Count),
		count_pairs(Count, Properties, CountPairs),
		fingerprint_pairs(Properties, FingerprintPairs),
		git_run_pairs(NormalizationContext, GitPairs),
		append(CountPairs, FingerprintPairs, Pairs0),
		append(Pairs0, GitPairs, Pairs).

	count_pairs(Count, Properties, [Key-Count]) :-
		member(count_key(Key), Properties),
		!.
	count_pairs(_Count, _Properties, []).

	fingerprint_pairs(Properties, [fingerprintAlgorithm-Algorithm]) :-
		member(fingerprint_algorithm(Algorithm), Properties),
		!.
	fingerprint_pairs(_Properties, []).

	git_run_pairs(repository(_RepositoryRoot, Branch, CommitHash), [gitBranch-Branch, gitCommitHash-CommitHash]) :-
		!.
	git_run_pairs(_NormalizationContext, []).

	normalization_context(_Target, Diagnostics, Properties, repository(RepositoryRoot, Branch, CommitHash)) :-
		memberchk(include_git_metadata(true), Properties),
		run_git_normalization_context(Diagnostics, RepositoryRoot, Branch, CommitHash),
		!.
	normalization_context(Target, Diagnostics, _Properties, application(ApplicationRoot)) :-
		application_root(Target, Diagnostics, ApplicationRoot),
		!.
	normalization_context(_Target, _Diagnostics, _Properties, none).

	run_git_normalization_context(Diagnostics, RepositoryRoot, Branch, CommitHash) :-
		setof(File, diagnostic_file(Diagnostics, File), [File| Files]),
		file_git_context(File, RepositoryRoot, Branch, CommitHash),
		same_git_context(Files, RepositoryRoot, CommitHash).

	file_git_context(File, RepositoryRoot, Branch, CommitHash) :-
		decompose_file_name(File, Directory, _BaseName),
		git_branch(Directory, Branch),
		git_commit_hash(Directory, CommitHash),
		git_repository_root(Directory, RepositoryRoot0),
		normalized_directory_prefix(RepositoryRoot0, RepositoryRoot).

	same_git_context([], _RepositoryRoot, _CommitHash).
	same_git_context([File| Files], RepositoryRoot, CommitHash) :-
		decompose_file_name(File, Directory, _BaseName),
		git_commit_hash(Directory, CommitHash),
		git_repository_root(Directory, RepositoryRoot0),
		normalized_directory_prefix(RepositoryRoot0, RepositoryRoot),
		same_git_context(Files, RepositoryRoot, CommitHash).

	application_root(Target, _Diagnostics, ApplicationRoot) :-
		target_application_root(Target, ApplicationRoot0),
		normalized_directory_prefix(ApplicationRoot0, ApplicationRoot),
		!.
	application_root(_Target, Diagnostics, ApplicationRoot) :-
		setof(Directory, diagnostic_directory(Diagnostics, Directory), Directories),
		common_directory_root(Directories, ApplicationRoot).

	target_application_root(directory(Directory), Directory).
	target_application_root(rdirectory(Directory), Directory).
	target_application_root(directories(Directories), Directory) :-
		common_directory_root(Directories, Directory).
	target_application_root(rdirectories(Directories), Directory) :-
		common_directory_root(Directories, Directory).
	target_application_root(library(Library), Directory) :-
		logtalk::expand_library_path(Library, Directory).
	target_application_root(rlibrary(Library), Directory) :-
		logtalk::expand_library_path(Library, Directory).
	target_application_root(libraries(Libraries), Directory) :-
		expanded_library_paths(Libraries, Directories),
		common_directory_root(Directories, Directory).
	target_application_root(rlibraries(Libraries), Directory) :-
		expanded_library_paths(Libraries, Directories),
		common_directory_root(Directories, Directory).

	expanded_library_paths([], []).
	expanded_library_paths([Library| Libraries], [Directory| Directories]) :-
		logtalk::expand_library_path(Library, Directory),
		expanded_library_paths(Libraries, Directories).

	diagnostic_file([diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, File0, _Lines, _Properties)| _], File) :-
		File0 \== '',
		normalized_internal_path(File0, File).
	diagnostic_file([_| Diagnostics], File) :-
		diagnostic_file(Diagnostics, File).

	diagnostic_directory([diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, File0, _Lines, _Properties)| _], Directory) :-
		File0 \== '',
		normalized_internal_path(File0, File),
		decompose_file_name(File, Directory0, _BaseName),
		normalized_directory_prefix(Directory0, Directory).
	diagnostic_directory([_| Diagnostics], Directory) :-
		diagnostic_directory(Diagnostics, Directory).

	common_directory_root([Directory| Directories], Root) :-
		normalized_directory_prefix(Directory, Prefix),
		common_directory_root(Directories, Prefix, Root).

	common_directory_root([], Root, Root).
	common_directory_root([Directory| Directories], Prefix0, Root) :-
		normalized_directory_prefix(Directory, Prefix1),
		common_directory_prefix(Prefix0, Prefix1, Prefix),
		common_directory_root(Directories, Prefix, Root).

	common_directory_prefix(Prefix, Directory, Prefix) :-
		atom_concat(Prefix, _, Directory),
		!.
	common_directory_prefix(Prefix0, Directory, Prefix) :-
		parent_directory_prefix(Prefix0, Parent),
		common_directory_prefix(Parent, Directory, Prefix).

	parent_directory_prefix(Path, Parent) :-
		strip_trailing_slash(Path, Path0),
		decompose_file_name(Path0, Parent0, _BaseName),
		Parent0 \== '',
		normalized_directory_prefix(Parent0, Parent),
		Parent \== Path.

	normalized_directory_prefix(Directory, Prefix) :-
		normalized_internal_path(Directory, Path0),
		(	sub_atom(Path0, _, 1, 0, /) ->
			Prefix = Path0
		;	atom_concat(Path0, '/', Prefix)
		).

	strip_trailing_slash(Path, Path0) :-
		(	Path \== ('/'),
			sub_atom(Path, _, 1, 0, /) ->
			sub_atom(Path, 0, _, 1, Path0)
		;	Path0 = Path
		).

	sarif_results([], _Rules, _ToolInfo, _NormalizationContext, []).
	sarif_results([Diagnostic| Diagnostics], Rules, ToolInfo, NormalizationContext, [Result| Results]) :-
		sarif_result(Diagnostic, Rules, ToolInfo, NormalizationContext, Result),
		sarif_results(Diagnostics, Rules, ToolInfo, NormalizationContext, Results).

	sarif_result(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Rules, ToolInfo, NormalizationContext, json(Pairs)) :-
		rule_index(RuleId, Rules, RuleIndex),
		message_pairs(Message, MessagePairs),
		location_pairs(File, Lines, NormalizationContext, LocationPairs),
		fingerprint_pairs(RuleId, File, Lines, Context, Properties, ToolInfo, NormalizationContext, FingerprintPairs),
		diagnostic_result_properties(RuleId, Confidence, Context, Properties, NormalizationContext, PropertiesJSON),
		Pairs0 = [
			ruleId-RuleId,
			ruleIndex-RuleIndex,
			level-Severity,
			message-json(MessagePairs)| LocationPairs
		],
		append(Pairs0, FingerprintPairs, Pairs1),
		append(Pairs1, [properties-PropertiesJSON], Pairs).

	message_pairs(Message, [text-Message]).

	location_pairs('', _Lines, _NormalizationContext, []).
	location_pairs(File, 0-0, NormalizationContext, [locations-[json([physicalLocation-json([artifactLocation-json(ArtifactLocationPairs)])])]]) :-
		artifact_location_pairs(File, NormalizationContext, ArtifactLocationPairs),
		!.
	location_pairs(File, Start-End, NormalizationContext, [locations-[json([physicalLocation-json([artifactLocation-json(ArtifactLocationPairs), region-json([startLine-Start, endLine-End])])])]]) :-
		artifact_location_pairs(File, NormalizationContext, ArtifactLocationPairs).

	artifact_location_pairs(File, repository(RepositoryRoot, _Branch, _CommitHash), [uri-RelativeURI, uriBaseId-'REPO_ROOT']) :-
		relative_artifact_uri(File, RepositoryRoot, RelativeURI),
		!.
	artifact_location_pairs(File, application(ApplicationRoot), [uri-RelativeURI, uriBaseId-'APP_ROOT']) :-
		relative_artifact_uri(File, ApplicationRoot, RelativeURI),
		!.
	artifact_location_pairs(File, _NormalizationContext, [uri-FileURI]) :-
		sarif_file_uri(File, FileURI).

	relative_artifact_uri(File0, Root, RelativeURI) :-
		normalized_internal_path(File0, File),
		atom_concat(Root, RelativeURI, File),
		RelativeURI \== ''.

	fingerprint_pairs(RuleId, File, Lines, Context, Properties, diagnostics_tool(_Id, _Name, _Version, _InformationURI, ToolProperties), NormalizationContext, [partialFingerprints-PartialFingerprints, fingerprints-Fingerprints]) :-
		normalized_location_identity(File, NormalizationContext, LocationIdentity),
		normalized_context_identity(Context, NormalizationContext, ContextIdentity),
		to_atom(rule_location(RuleId, LocationIdentity, Lines), RuleLocationFingerprint),
		to_atom(context(ContextIdentity), ContextFingerprint),
		stable_diagnostic_identity(RuleId, LocationIdentity, Lines, ContextIdentity, Properties, DiagnosticIdentity),
		to_atom(DiagnosticIdentity, CanonicalDiagnosticFingerprint),
		canonical_fingerprint_key(ToolProperties, CanonicalFingerprintKey),
		(	memberchk(entity_kind(EntityKind), Properties),
			memberchk(entity(Entity), Properties),
			memberchk(predicate(Predicate), Properties) ->
			to_atom(diagnostic(EntityKind, Entity, Predicate), EntityPredicateFingerprint),
			PartialFingerprints = json([entityPredicateV1-EntityPredicateFingerprint, locationV1-RuleLocationFingerprint])
		;	PartialFingerprints = json([ruleLocationV1-RuleLocationFingerprint, contextV1-ContextFingerprint])
		),
		Fingerprints = json([CanonicalFingerprintKey-CanonicalDiagnosticFingerprint]).

	normalized_location_identity('', _NormalizationContext, unavailable) :-
		!.
	normalized_location_identity(File, repository(RepositoryRoot, _Branch, _CommitHash), relative('REPO_ROOT', RelativeURI)) :-
		relative_artifact_uri(File, RepositoryRoot, RelativeURI),
		!.
	normalized_location_identity(File, application(ApplicationRoot), relative('APP_ROOT', RelativeURI)) :-
		relative_artifact_uri(File, ApplicationRoot, RelativeURI),
		!.
	normalized_location_identity(File, _NormalizationContext, absolute(FileURI)) :-
		sarif_file_uri(File, FileURI).

	normalized_context_identity(context(file, Identifier), NormalizationContext, context(file, LocationIdentity)) :-
		Identifier \== '',
		normalized_location_identity(Identifier, NormalizationContext, LocationIdentity),
		!.
	normalized_context_identity(Context, _NormalizationContext, Context).

	stable_diagnostic_identity(RuleId, LocationIdentity, _Lines, _ContextIdentity, Properties, finding(RuleId, LocationIdentity, EntityKind, Entity, Predicate)) :-
		memberchk(entity_kind(EntityKind), Properties),
		memberchk(entity(Entity), Properties),
		memberchk(predicate(Predicate), Properties),
		!.
	stable_diagnostic_identity(RuleId, LocationIdentity, Lines, ContextIdentity, Properties, warning(RuleId, LocationIdentity, Lines, ContextIdentity, StableProperties)) :-
		stable_identity_properties(Properties, StableProperties).

	stable_identity_properties([], []).
	stable_identity_properties([Property| Properties], [Property| StableProperties]) :-
		stable_identity_property(Property),
		!,
		stable_identity_properties(Properties, StableProperties).
	stable_identity_properties([_| Properties], StableProperties) :-
		stable_identity_properties(Properties, StableProperties).

	stable_identity_property(entity_kind(_)).
	stable_identity_property(entity(_)).
	stable_identity_property(predicate(_)).
	stable_identity_property(flag(_)).
	stable_identity_property(directive(_)).
	stable_identity_property(indicator(_)).
	stable_identity_property(key(_)).
	stable_identity_property(test(_)).
	stable_identity_property(option(_)).
	stable_identity_property(prerequisite(_)).
	stable_identity_property(auxiliary_clause(_)).

	canonical_fingerprint_key(Properties, canonicalFindingV1) :-
		member(fingerprint_algorithm(canonical_finding_v1), Properties),
		!.
	canonical_fingerprint_key(_Properties, canonicalWarningV1).

	diagnostic_result_properties(RuleId, Confidence, _Context, Properties, _NormalizationContext, json(Pairs)) :-
		memberchk(entity_kind(EntityKind), Properties),
		memberchk(entity(Entity), Properties),
		memberchk(predicate(Predicate), Properties),
		(	member(finding_properties(FindingProperties), Properties) ->
			terms_to_atoms(FindingProperties, FindingPropertiesJSON)
		;	FindingPropertiesJSON = []
		),
		to_atom(Entity, EntityAtom),
		to_atom(Predicate, PredicateAtom),
		Pairs = [
			class-RuleId,
			confidence-Confidence,
			findingProperties-FindingPropertiesJSON,
			entityKind-EntityKind,
			entity-EntityAtom,
			predicate-PredicateAtom
		],
		!.
	diagnostic_result_properties(_RuleId, _Confidence, Context, Properties, NormalizationContext, json(Pairs)) :-
		memberchk(flag(Flag), Properties),
		normalized_context_identity(Context, NormalizationContext, ContextIdentity),
		to_atom(ContextIdentity, ContextAtom),
		memberchk(raw_term(RawTerm), Properties),
		to_atom(RawTerm, RawTermAtom),
		memberchk(details(Details), Properties),
		terms_to_atoms(Details, DetailsAtoms),
		terms_to_atoms(Properties, PropertyAtoms),
		(	member(auxiliary_clause(true), Properties) ->
			AuxiliaryClause = @true
		;	AuxiliaryClause = @false
		),
		(	member(explanation(Explanation), Properties) ->
			HasExplanation = @true,
			ExplanationText = Explanation
		;	HasExplanation = @false,
			ExplanationText = ''
		),
		Pairs = [
			flag-Flag,
			context-ContextAtom,
			rawTerm-RawTermAtom,
			details-DetailsAtoms,
			auxiliaryClause-AuxiliaryClause,
			hasExplanation-HasExplanation,
			explanation-ExplanationText,
			warningProperties-PropertyAtoms
		],
		!.
	diagnostic_result_properties(_RuleId, Confidence, Context, Properties, NormalizationContext, json([
		confidence-Confidence,
		context-ContextAtom,
		diagnosticProperties-PropertyAtoms
	])) :-
		normalized_context_identity(Context, NormalizationContext, ContextIdentity),
		to_atom(ContextIdentity, ContextAtom),
		terms_to_atoms(Properties, PropertyAtoms).

	sarif_notifications([], _NormalizationContext, []).
	sarif_notifications([Issue| Issues], NormalizationContext, [Notification| Notifications]) :-
		sarif_notification(Issue, NormalizationContext, Notification),
		sarif_notifications(Issues, NormalizationContext, Notifications).

	sarif_notification(preflight_issue(Id, Severity, Message, _Context, File, Lines, Properties), NormalizationContext, json(Pairs)) :-
		message_pairs(Message, MessagePairs),
		location_pairs(File, Lines, NormalizationContext, LocationPairs),
		preflight_notification_properties(Id, Severity, File, Properties, NormalizationContext, PropertiesJSON),
		Pairs0 = [level-Severity, message-json(MessagePairs)| LocationPairs],
		append(Pairs0, [properties-PropertiesJSON], Pairs).

	preflight_notification_properties(Id, Severity, File, Properties, NormalizationContext, json(Pairs)) :-
		normalized_file_atom(File, NormalizationContext, FileAtom),
		(	member(prerequisite(Prerequisite), Properties) ->
			terms_to_atoms(Properties, PropertyAtoms),
			Pairs = [kind-Id, prerequisite-Prerequisite, severity-Severity, file-FileAtom, preflightProperties-PropertyAtoms]
		;	terms_to_atoms(Properties, PropertyAtoms),
			Pairs = [kind-Id, severity-Severity, file-FileAtom, preflightProperties-PropertyAtoms]
		).

	normalized_file_atom('', _NormalizationContext, '') :-
		!.
	normalized_file_atom(File, NormalizationContext, FileAtom) :-
		normalized_location_identity(File, NormalizationContext, FileIdentity),
		to_atom(FileIdentity, FileAtom).

	rule_index(RuleId, [diagnostic_rule(RuleId, _ShortDescription, _FullDescription, _DefaultSeverity, _Properties)| _], 0) :-
		!.
	rule_index(RuleId, [_| Rules], RuleIndex) :-
		rule_index(RuleId, Rules, RuleIndex0),
		RuleIndex is RuleIndex0 + 1.

	sarif_version_control_provenance(Diagnostics, VersionControlProvenance) :-
		(	setof(Directory, sarif_version_control_directory(Diagnostics, Directory), Directories) ->
			sarif_version_control_details(Directories, VersionControlProvenance)
		;	VersionControlProvenance = []
		).

	sarif_version_control_directory([diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, File, _Lines, _Properties)| _], Directory) :-
		File \== '',
		normalized_internal_path(File, InternalFile),
		decompose_file_name(InternalFile, Directory, _BaseName).
	sarif_version_control_directory([_| Diagnostics], Directory) :-
		sarif_version_control_directory(Diagnostics, Directory).

	sarif_version_control_details([], []).
	sarif_version_control_details([Directory| Directories], [json([
		repositoryUri-RepositoryURI,
		revisionId-CommitHash,
		branch-Branch,
		mappedTo-json([uri-RepositoryRootURI])
	])| VersionControlProvenance]) :-
		git_branch(Directory, Branch),
		git_commit_hash(Directory, CommitHash),
		git_repository_root(Directory, RepositoryRoot),
		git_repository_uri(Directory, RepositoryURI),
		sarif_directory_uri(RepositoryRoot, RepositoryRootURI),
		!,
		sarif_version_control_details(Directories, VersionControlProvenance).
	sarif_version_control_details([_| Directories], VersionControlProvenance) :-
		sarif_version_control_details(Directories, VersionControlProvenance).

	git_repository_root(Directory, RepositoryRoot) :-
		git_command_clean_output(Directory, 'rev-parse --show-toplevel', RepositoryRoot),
		RepositoryRoot \== ''.

	git_repository_uri(Directory, RepositoryURI) :-
		git_command_clean_output(Directory, 'config --get remote.origin.url', RemoteURI),
		RemoteURI \== '',
		normalize_git_repository_uri(RemoteURI, RepositoryURI).

	normalize_git_repository_uri(RemoteURI, RepositoryURI) :-
		(	scp_like_git_repository_uri(RemoteURI, URI) ->
			true
		;	URI = RemoteURI
		),
		url_normalize(URI, RepositoryURI).

	scp_like_git_repository_uri(RemoteURI, RepositoryURI) :-
		sub_atom(RemoteURI, Before, 1, After, :),
		Before > 0,
		After > 0,
		sub_atom(RemoteURI, 0, Before, _, UserHost),
		sub_atom(RemoteURI, _, After, 0, Path),
		sub_atom(UserHost, _, _, _, @),
		atomic_list_concat(['ssh://', UserHost, '/', Path], RepositoryURI).

	sarif_directory_uri(Directory, URI) :-
		normalized_internal_path(Directory, Path0),
		ensure_directory_path(Path0, Path),
		url_file_path_components(Path, Components0),
		url_generate([scheme(file)| Components0], URI).

	sarif_file_uri(File, URI) :-
		normalized_internal_path(File, Path0),
		url_file_path_components(Path0, Components0),
		url_generate([scheme(file)| Components0], URI).

	ensure_directory_path(Path0, Path) :-
		(	sub_atom(Path0, _, 1, 0, /) ->
			Path = Path0
		;	atom_concat(Path0, '/', Path)
		).

	normalized_internal_path(Path0, Path) :-
		internal_os_path(InternalPath, Path0),
		normalize_path_separators(InternalPath, Path).

	normalize_path_separators(Path0, Path) :-
		atom_codes(Path0, Codes0),
		normalize_path_separator_codes(Codes0, Codes),
		atom_codes(Path, Codes).

	normalize_path_separator_codes([], []).
	normalize_path_separator_codes([0'\\| Codes0], [0'/| Codes]) :-
		!,
		normalize_path_separator_codes(Codes0, Codes).
	normalize_path_separator_codes([Code| Codes0], [Code| Codes]) :-
		normalize_path_separator_codes(Codes0, Codes).

	:- if(os::operating_system_type(windows)).

		git_command_clean_output(Directory, GitCommand, Output) :-
			temporary_file(Temporary),
			internal_os_path(Temporary, NativeTemporary),
			atomic_list_concat(['git -C "', Directory, '" ', GitCommand, ' 2>nul > "', NativeTemporary, '"'], Command),
			(	shell(Command) ->
				open(Temporary, read, Stream),
				line_to_codes(Stream, OutputCodes),
				close(Stream),
				atom_codes(Output, OutputCodes),
				delete_file(Temporary)
			;	delete_file(Temporary),
				fail
			).

	:- else.

		git_command_clean_output(Directory, GitCommand, Output) :-
			temporary_file(Temporary),
			atomic_list_concat(['git -C "', Directory, '" ', GitCommand, ' 2>/dev/null > "', Temporary, '"'], Command),
			(	shell(Command) ->
				open(Temporary, read, Stream),
				line_to_codes(Stream, OutputCodes),
				close(Stream),
				atom_codes(Output, OutputCodes),
				delete_file(Temporary)
			;	delete_file(Temporary),
				fail
			).

	:- endif.

	temporary_file(Temporary) :-
		pid(PID),
		atomic_list_concat([logtalk_sarif_git_, PID], Basename),
		temporary_directory(Directory),
		path_concat(Directory, Basename, Temporary).

	terms_to_atoms([], []).
	terms_to_atoms([Term| Terms], [Atom| Atoms]) :-
		to_atom(Term, Atom),
		terms_to_atoms(Terms, Atoms).

	normalize_json_term(json([]), {}) :-
		!.
	normalize_json_term(json(Pairs0), {Pairs}) :-
		!,
		normalize_json_pairs(Pairs0, Pairs).
	normalize_json_term([Term0| Terms0], [Term| Terms]) :-
		!,
		normalize_json_term(Term0, Term),
		normalize_json_term(Terms0, Terms).
	normalize_json_term(Term, Term).

	normalize_json_pairs([], []).
	normalize_json_pairs([Key-Value0| Pairs0], [Key-Value| Pairs]) :-
		normalize_json_term(Value0, Value),
		normalize_json_pairs(Pairs0, Pairs).

	to_atom(Term, Atom) :-
		(	atom(Term) ->
			Atom = Term
		;	copy_term(Term, Copy),
			numbervars(Copy, 0, _),
			write_term_to_atom(Copy, Atom, [numbervars(true), quoted(true)])
		).

:- end_object.
