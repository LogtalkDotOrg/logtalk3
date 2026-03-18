%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2016 Barry Evans <barryevans@kyndi.com>
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


:- object(dead_code_scanner,
	imports(options)).

	:- info([
		version is 0:16:0,
		author is 'Barry Evans and Paulo Moura',
		date is 2026-03-18,
		comment is 'A tool for detecting *likely* dead code in compiled Logtalk entities and Prolog modules compiled as objects.',
		remarks is [
			'Dead code' - 'A predicate or non-terminal that is not called (directly or indirectly) by any scoped predicate or non-terminal. These predicates and non-terminals are not used, cannot be called without breaking encapsulation, and are thus considered dead code.',
			'Known issues' - 'Use of local meta-calls with goal arguments only know at runtime can result in false positives. Calls from non-standard meta-predicates may be missed if the meta-calls are not optimized.',
			'Requirements' - 'Source files must be compiled with the ``source_data`` flag turned on. To avoid false positives do to meta-calls, compilation of source files with the ``optimized`` flag turned on is also advised.'
		]
	]).

	:- public(entity/1).
	:- mode(entity(+entity_identifier), zero_or_one).
	:- info(entity/1, [
		comment is 'Scans a loaded entity for dead code. Fails if the entity does not exist.',
		argnames is ['Entity']
	]).

	:- public(entity/2).
	:- mode(entity(+entity_identifier, +list(compound)), zero_or_one).
	:- info(entity/2, [
		comment is 'Scans a loaded entity for dead code using the given options. Fails if the entity does not exist.',
		argnames is ['Entity', 'Options']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list(compound)), zero_or_one).
	:- info(file/2, [
		comment is 'Scans all entities in a loaded source file for dead code using the given options. The file can be given by name, basename, full path, or using library notation. Fails if the file is not loaded.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), zero_or_one).
	:- info(file/1, [
		comment is 'Scans all entities in a loaded source file for dead code using default options. The file can be given by name, basename, full path, or using library notation. Fails if the file is not loaded.',
		argnames is ['File']
	]).

	:- public(directory/2).
	:- mode(directory(+atom, +list(compound)), one).
	:- info(directory/2, [
		comment is 'Scans all entities in all loaded files from a given directory for dead code using the given options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Scans all entities in all loaded files from a given directory for dead code using default options.',
		argnames is ['Directory']
	]).

	:- public(rdirectory/2).
	:- mode(rdirectory(+atom, +list(compound)), one).
	:- info(rdirectory/2, [
		comment is 'Scans all entities in all loaded files from a given directory and its sub-directories for dead code using the given options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Scans all entities in all loaded files from a given directory and its sub-directories for dead code using default options.',
		argnames is ['Directory']
	]).

	:- public(library/2).
	:- mode(library(+atom, +list(compound)), one).
	:- info(library/2, [
		comment is 'Scans all entities in all loaded files from a given library for dead code using the given options.',
		argnames is ['Library', 'Options']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Scans all entities in all loaded files from a given library for dead code using default options.',
		argnames is ['Library']
	]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list(compound)), one).
	:- info(rlibrary/2, [
		comment is 'Scans all entities in all loaded files in a loaded library and its sub-libraries for dead code using the given options.',
		argnames is ['Library', 'Options']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Scans all entities in all loaded files in a loaded library and its sub-libraries for dead code using default options.',
		argnames is ['Library']
	]).

	:- public(all/1).
	:- mode(all(+list(compound)), one).
	:- info(all/1, [
		comment is 'Scans all entities for dead code using the given options.',
		argnames is ['Options']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Scans all entities for dead code using default options.'
	]).

	:- public(findings/3).
	:- mode(findings(+nonvar, -list(compound), +list(compound)), one).
	:- info(findings/3, [
		comment is 'Returns an ordered set of findings for a scan target using the given options. Supported targets are ``all`` and the compound terms ``entity/1``, ``file/1``, ``directory/1``, ``rdirectory/1``, ``library/1``, and ``rlibrary/1``.',
		argnames is ['Target', 'Findings', 'Options']
	]).

	:- public(findings/2).
	:- mode(findings(+nonvar, -list(compound)), one).
	:- info(findings/2, [
		comment is 'Returns an ordered set of findings for a scan target using default options. Supported targets are ``all`` and the compound terms ``entity/1``, ``file/1``, ``directory/1``, ``rdirectory/1``, ``library/1``, and ``rlibrary/1``.',
		argnames is ['Target', 'Findings']
	]).

	:- public(finding/3).
	:- mode(finding(+nonvar, -compound, +list(compound)), zero_or_more).
	:- info(finding/3, [
		comment is 'Enumerates, by backtracking, findings for a scan target using the given options. Supported targets are ``all`` and the compound terms ``entity/1``, ``file/1``, ``directory/1``, ``rdirectory/1``, ``library/1``, and ``rlibrary/1``.',
		argnames is ['Target', 'Finding', 'Options']
	]).

	:- public(finding/2).
	:- mode(finding(+nonvar, -compound), zero_or_more).
	:- info(finding/2, [
		comment is 'Enumerates, by backtracking, findings for a scan target using default options. Supported targets are ``all`` and the compound terms ``entity/1``, ``file/1``, ``directory/1``, ``rdirectory/1``, ``library/1``, and ``rlibrary/1``.',
		argnames is ['Target', 'Finding']
	]).

	:- public(summary/3).
	:- mode(summary(+nonvar, -compound, +list(compound)), one).
	:- info(summary/3, [
		comment is 'Returns a machine-readable summary for a scan target using the given options. The summary term is of the form ``summary(Target, TotalEntities, TotalFindings, EntitySummaries)`` where ``EntitySummaries`` is a list of ``entity_summary(Kind, Entity, FindingsCount)`` terms.',
		argnames is ['Target', 'Summary', 'Options']
	]).

	:- public(summary/2).
	:- mode(summary(+nonvar, -compound), one).
	:- info(summary/2, [
		comment is 'Returns a machine-readable summary for a scan target using default options. The summary term is of the form ``summary(Target, TotalEntities, TotalFindings, EntitySummaries)`` where ``EntitySummaries`` is a list of ``entity_summary(Kind, Entity, FindingsCount)`` terms.',
		argnames is ['Target', 'Summary']
	]).

	:- public(export/4).
	:- mode(export(+nonvar, +atom, ++compound, +list(compound)), one).
	:- info(export/4, [
		comment is 'Exports a scan target using the given options in the specified format to the specified sink. Supported formats are ``json`` and ``sarif``. Supported sinks are those accepted by the ``json`` library ``generate/2`` predicate.',
		argnames is ['Target', 'Format', 'Sink', 'Options']
	]).

	:- public(export/3).
	:- mode(export(+nonvar, +atom, ++compound), one).
	:- info(export/3, [
		comment is 'Exports a scan target using default options in the specified format to the specified sink. Supported formats are ``json`` and ``sarif``. Supported sinks are those accepted by the ``json`` library ``generate/2`` predicate.',
		argnames is ['Target', 'Format', 'Sink']
	]).

	:- public(predicates/2).
	:- mode(predicates(+entity_identifier, -list(predicate_indicator)), one).
	:- info(predicates/2, [
		comment is 'Returns an ordered set of local predicates (and non-terminals) that are not used, directly or indirectly, by scoped predicates for a loaded entity.',
		argnames is ['Entity', 'Predicates']
	]).

	:- public(predicates/3).
	:- mode(predicates(+entity_identifier, -list(predicate_indicator), +list(compound)), one).
	:- info(predicates/3, [
		comment is 'Returns an ordered set of local predicates (and non-terminals) that are not used, directly or indirectly, by scoped predicates for a loaded entity using the given options.',
		argnames is ['Entity', 'Predicates', 'Options']
	]).

	:- public(predicate/2).
	:- mode(predicate(+entity_identifier, ?predicate_indicator), zero_or_more).
	:- info(predicate/2, [
		comment is 'Enumerates, by backtracking, local predicates (and non-terminals) that are not used, directly or indirectly, by scoped predicates for a loaded entity.',
		argnames is ['Entity', 'Predicate']
	]).

	:- public(predicate/3).
	:- mode(predicate(+entity_identifier, ?predicate_indicator, +list(compound)), zero_or_more).
	:- info(predicate/3, [
		comment is 'Enumerates, by backtracking, local predicates (and non-terminals) that are not used, directly or indirectly, by scoped predicates for a loaded entity using the given options.',
		argnames is ['Entity', 'Predicate', 'Options']
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(json_schema, [
		parse/2 as json_schema_parse/2,
		validate/2 as json_schema_validate/2
	]).

	:- uses(git, [
		branch/2 as git_branch/2,
		commit_hash/2 as git_commit_hash/2
	]).

	% Use the structured printing mechanism in order to allow results to be
	% intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [
		expand_library_path/2, file_type_extension/2, loaded_file_property/2, print_message/3
	]).

	:- uses(os, [
		date_time/7,
		decompose_file_name/3, decompose_file_name/4,
		absolute_file_name/2, delete_file/1, directory_exists/1,
		internal_os_path/2, path_concat/3, pid/1, shell/1,
		temporary_directory/1
	]).

	:- uses(reader, [
		line_to_codes/2
	]).

	:- uses(term_io, [
		write_term_to_atom/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(url(atom), [
		generate/2 as url_generate/2,
		normalize/2 as url_normalize/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- uses(uuid, [
		uuid_v4/1
	]).

	findings(Target, Findings, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	setof(Finding, finding(Target, Finding, Options), Findings) ->
			true
		;	Findings = []
		).

	findings(Target, Findings) :-
		findings(Target, Findings, []).

	finding(Target, Finding, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		target_entity(Target, Kind, Entity, Options),
		dead_code_finding(Kind, Entity, Finding, Options),
		\+ waived_finding(Finding, Options).

	finding(Target, Finding) :-
		finding(Target, Finding, []).

	summary(Target, summary(Target, TotalEntities, TotalFindings, EntitySummaries), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		target_entities(Target, Entities, Options),
		length(Entities, TotalEntities),
		entity_summaries(Entities, EntitySummaries, Options),
		sum_entity_findings(EntitySummaries, TotalFindings).

	summary(Target, Summary) :-
		summary(Target, Summary, []).

	export_term(json, Target, JSON, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		findings(Target, Findings, Options),
		summary(Target, Summary, Options),
		target_json(Target, TargetJSON),
		options_json(Options, OptionsJSON),
		summary_json(Summary, SummaryJSON),
		findings_json(Findings, FindingsJSON),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-TargetJSON,
			options-OptionsJSON,
			summary-SummaryJSON,
			findings-FindingsJSON
		}.
	export_term(sarif, Target, SARIF, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		findings(Target, Findings, Options),
		target_sarif(Target, TargetSARIF),
		uuid_v4(RunGUID),
		sarif_run_properties(Findings, RunProperties),
		sarif_run_version_control_provenance(Findings, VersionControlProvenance),
		sarif_results(Findings, Results),
		sarif_tool_driver(Driver),
		sarif_run(TargetSARIF, RunGUID, Driver, VersionControlProvenance, RunProperties, Results, Run),
		SARIF = {
			'$schema'-'https://json.schemastore.org/sarif-2.1.0.json',
			version-'2.1.0',
			runs-[Run]
		}.

	export(Target, Format, Sink, UserOptions) :-
		( 	Format == json ->
			export_term(json, Target, Term, UserOptions),
			validate_export_term(json, Term, UserOptions),
			json::generate(Sink, Term)
		;	Format == sarif ->
			export_term(sarif, Target, Term, UserOptions),
			validate_export_term(sarif, Term, UserOptions),
			json::generate(Sink, Term)
		;	fail
		).

	export(Target, Format, Sink) :-
		export(Target, Format, Sink, []).

	predicates(Entity, Predicates, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	setof(Predicate, predicate(Entity, Predicate, Options), Predicates) ->
			true
		;	Predicates = []
		).

	predicates(Entity, Predicates) :-
		predicates(Entity, Predicates, []).

	predicate(Entity, Predicate, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		\+ (atom(Entity), current_protocol(Entity)),
		predicate(Entity, Predicate, _, _, Options).

	predicate(Entity, Predicate) :-
		predicate(Entity, Predicate, []).

	% local predicates not called, directly or indirectly, by scoped predicates
	dead_code_finding(Kind, Entity, dead_predicate(Kind, Entity, Predicate, File, Lines), Options) :-
		predicate(Entity, Predicate, File, Lines, Options).

	target_entities(Target, Entities, Options) :-
		(	setof(entity(Kind, Entity), target_entity(Target, Kind, Entity, Options), Entities) ->
			true
		;	Entities = []
		).

	entity_summaries([], [], _).
	entity_summaries([entity(Kind, Entity)| Entities], [entity_summary(Kind, Entity, FindingsCount)| EntitySummaries], Options) :-
		entity_findings_count(Kind, Entity, FindingsCount, Options),
		entity_summaries(Entities, EntitySummaries, Options).

	entity_findings_count(Kind, Entity, FindingsCount, Options) :-
		findall(
			Finding,
			(	dead_code_finding(Kind, Entity, Finding, Options),
				\+ waived_finding(Finding, Options)
			),
			Findings
		),
		length(Findings, FindingsCount).

	sum_entity_findings([], 0).
	sum_entity_findings([entity_summary(_, _, FindingsCount)| EntitySummaries], TotalFindings) :-
		sum_entity_findings(EntitySummaries, TotalFindings0),
		TotalFindings is FindingsCount + TotalFindings0.

	target_json(all, {kind-all, value-all}) :-
		!.
	target_json(entity(Entity), {kind-entity, value-Atom}) :-
		!,
		to_atom(Entity, Atom).
	target_json(file(File), {kind-file, value-Atom}) :-
		!,
		to_atom(File, Atom).
	target_json(directory(Directory), {kind-directory, value-Atom}) :-
		!,
		to_atom(Directory, Atom).
	target_json(rdirectory(Directory), {kind-rdirectory, value-Atom}) :-
		!,
		to_atom(Directory, Atom).
	target_json(library(Library), {kind-library, value-Atom}) :-
		!,
		to_atom(Library, Atom).
	target_json(rlibrary(Library), {kind-rlibrary, value-Atom}) :-
		!,
		to_atom(Library, Atom).
	target_json(Target, {kind-custom, value-Atom}) :-
		to_atom(Target, Atom).

	options_json(Options, {
		excludeDirectories-ExcludeDirectories,
		excludeFiles-ExcludeFiles,
		excludeEntities-ExcludeEntities,
		excludePredicates-ExcludePredicates,
		excludeLibraries-ExcludeLibraries,
		waiveFindings-WaiveFindings,
		validateExport-ValidateExportJSON
	}) :-
		^^option(exclude_directories(ExcludeDirectories0), Options),
		^^option(exclude_files(ExcludeFiles0), Options),
		^^option(exclude_entities(ExcludeEntities0), Options),
		^^option(exclude_predicates(ExcludePredicates0), Options),
		^^option(exclude_libraries(ExcludeLibraries0), Options),
		^^option(waive_findings(WaiveFindings0), Options),
		^^option(validate_export(ValidateExport), Options),
		boolean_json(ValidateExport, ValidateExportJSON),
		terms_to_atoms(ExcludeDirectories0, ExcludeDirectories),
		terms_to_atoms(ExcludeFiles0, ExcludeFiles),
		terms_to_atoms(ExcludeEntities0, ExcludeEntities),
		terms_to_atoms(ExcludePredicates0, ExcludePredicates),
		terms_to_atoms(ExcludeLibraries0, ExcludeLibraries),
		terms_to_atoms(WaiveFindings0, WaiveFindings).

	summary_json(summary(_, TotalEntities, TotalFindings, EntitySummaries), {
		totalEntities-TotalEntities,
		totalFindings-TotalFindings,
		entities-EntitiesJSON
	}) :-
		entity_summaries_json(EntitySummaries, EntitiesJSON).

	entity_summaries_json([], []).
	entity_summaries_json([EntitySummary| EntitySummaries], [EntitySummaryJSON| EntitiesJSON]) :-
		entity_summary_json(EntitySummary, EntitySummaryJSON),
		entity_summaries_json(EntitySummaries, EntitiesJSON).

	entity_summary_json(entity_summary(Kind, Entity, FindingsCount), {
		kind-Kind,
		entity-EntityAtom,
		findingsCount-FindingsCount
	}) :-
		to_atom(Entity, EntityAtom).

	findings_json([], []).
	findings_json([Finding| Findings], [FindingJSON| FindingsJSON]) :-
		finding_json(Finding, FindingJSON),
		findings_json(Findings, FindingsJSON).

	finding_json(dead_predicate(EntityKind, Entity, Predicate, File, Start-End), {
		kind-dead_predicate,
		entityKind-EntityKind,
		entity-EntityAtom,
		predicate-PredicateAtom,
		file-FileAtom,
		lines-{start-Start, end-End}
	}) :-
		to_atom(Entity, EntityAtom),
		to_atom(Predicate, PredicateAtom),
		to_atom(File, FileAtom).

	terms_to_atoms([], []).
	terms_to_atoms([Term| Terms], [Atom| Atoms]) :-
		to_atom(Term, Atom),
		terms_to_atoms(Terms, Atoms).

	to_atom(Term, Atom) :-
		(	atom(Term) ->
			Atom = Term
		;	copy_term(Term, Copy),
			numbervars(Copy, 0, _),
			write_term_to_atom(Copy, Atom, [numbervars(true), quoted(true)])
		).

	boolean_json(true, @true).
	boolean_json(false, @false).

	validate_export_term(json, JSON, UserOptions) :-
		!,
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(validate_export(Validate), Options),
		( 	Validate == true ->
			export_schema_path(Path),
			json_schema_parse(file(Path), Schema),
			json_schema_validate(Schema, JSON)
		;	true
		).
	validate_export_term(sarif, _SARIF, _UserOptions).

	export_schema_path(Path) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'dead_code_scanner.schema.json', Path).

	target_sarif(all, all) :-
		!.
	target_sarif(Target, Atom) :-
		target_json(Target, {kind-Kind, value-Value}),
		to_atom(Kind-Value, Atom).

	sarif_tool_driver({
		name-dead_code_scanner,
		informationUri-'https://logtalk.org/',
		version-'0.22.0',
		guid-'91f50eb3-a092-43b5-b8e2-3c1f64bb7047',
		rules-[{
			id-dead_predicate,
			guid-'f6fd0e53-0c2d-45fd-a6dd-7b2f2af3e2a1',
			name-dead_predicate,
			shortDescription-{text-'Likely dead predicate or non-terminal.'},
			fullDescription-{text-'Predicate or non-terminal appears to be unused after applying exclusions and waivers.'}
		}]
	}).

	sarif_run(TargetSARIF, RunGUID, Driver, [VersionControlDetails| VersionControlProvenance], RunProperties, Results, {
		tool-{driver-Driver},
		automationDetails-{id-TargetSARIF, guid-RunGUID},
		invocations-[{executionSuccessful- @true}],
		versionControlProvenance-[VersionControlDetails| VersionControlProvenance],
		properties-RunProperties,
		results-Results
	}) :-
		!.
	sarif_run(TargetSARIF, RunGUID, Driver, [], RunProperties, Results, {
		tool-{driver-Driver},
		automationDetails-{id-TargetSARIF, guid-RunGUID},
		invocations-[{executionSuccessful- @true}],
		properties-RunProperties,
		results-Results
	}).

	sarif_run_properties(Findings, {
		fingerprintAlgorithm-canonical_finding_v1,
		gitBranch-Branch,
		gitCommitHash-CommitHash
	}) :-
		sarif_git_metadata(Findings, Branch, CommitHash),
		!.
	sarif_run_properties(_Findings, {
		fingerprintAlgorithm-canonical_finding_v1
	}).

	sarif_git_metadata([dead_predicate(_, _, _, File, _)| _], Branch, CommitHash) :-
		decompose_file_name(File, Directory, _, _),
		git_branch(Directory, Branch),
		git_commit_hash(Directory, CommitHash).

	sarif_run_version_control_provenance(Findings, VersionControlProvenance) :-
		(	setof(Directory, sarif_version_control_directory(Findings, Directory), Directories) ->
			sarif_version_control_details(Directories, VersionControlProvenance)
		;	VersionControlProvenance = []
		).

	sarif_version_control_directory([dead_predicate(_, _, _, File, _)| _], Directory) :-
		decompose_file_name(File, Directory, _, _).
	sarif_version_control_directory([_| Findings], Directory) :-
		sarif_version_control_directory(Findings, Directory).

	sarif_version_control_details([], []).
	sarif_version_control_details([Directory| Directories], [VersionControlDetails| VersionControlProvenance]) :-
		VersionControlDetails = {
			repositoryUri-RepositoryURI,
			revisionId-CommitHash,
			branch-Branch,
			mappedTo-{uri-RepositoryRootURI}
		},
		git_branch(Directory, Branch),
		git_commit_hash(Directory, CommitHash),
		git_repository_root(Directory, RepositoryRoot),
		git_repository_uri(Directory, RepositoryURI),
		sarif_directory_uri(RepositoryRoot, RepositoryRootURI),
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
		url_normalize(URI, RepositoryURI),
		!.

	scp_like_git_repository_uri(RemoteURI, RepositoryURI) :-
		sub_atom(RemoteURI, Before, 1, After, :),
		Before > 0,
		After > 0,
		sub_atom(RemoteURI, 0, Before, _, UserHost),
		sub_atom(RemoteURI, _, After, 0, Path),
		sub_atom(UserHost, _, _, _, @),
		atomic_list_concat(['ssh://', UserHost, '/', Path], RepositoryURI).

	sarif_directory_uri(Directory, URI) :-
		internal_os_path(Directory, Path0),
		directory_file_url_path(Path0, Path),
		url_generate([scheme(file), authority(''), path(Path)], URI0),
		url_normalize(URI0, URI),
		!.

	directory_file_url_path(Path0, Path) :-
		file_url_path(Path0, Path1),
		(	sub_atom(Path1, _, 1, 0, /) ->
			Path = Path1
		;	atom_concat(Path1, '/', Path)
		).

	file_url_path(Path0, Path) :-
		(	sub_atom(Path0, 0, 1, _, /) ->
			Path = Path0
		;	sub_atom(Path0, 1, 2, _, ':/') ->
			atom_concat('/', Path0, Path)
		;	Path = Path0
		).

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
		atomic_list_concat([logtalk_dead_code_scanner_git_, PID], Basename),
		temporary_directory(Directory),
		path_concat(Directory, Basename, Temporary).

	sarif_results([], []).
	sarif_results([Finding| Findings], [Result| Results]) :-
		sarif_result(Finding, Result),
		sarif_results(Findings, Results).

	sarif_result(dead_predicate(EntityKind, Entity, Predicate, File, Start-End), {
		ruleId-dead_predicate,
		ruleIndex-0,
		level-warning,
		message-{text-Message},
		locations-[{physicalLocation-{
			artifactLocation-{uri-FileURI},
			region-{startLine-Start, endLine-End}
		}}],
		partialFingerprints-PartialFingerprints,
		fingerprints-Fingerprints,
		properties-{
			entityKind-EntityKind,
			entity-EntityAtom,
			predicate-PredicateAtom
		}
	}) :-
		to_atom(Entity, EntityAtom),
		to_atom(Predicate, PredicateAtom),
		to_atom(File, FileAtom),
		sarif_message(EntityKind, EntityAtom, PredicateAtom, Message),
		sarif_file_uri(FileAtom, FileURI),
		sarif_fingerprints(EntityKind, EntityAtom, PredicateAtom, FileURI, Start-End, PartialFingerprints, Fingerprints).

	sarif_fingerprints(EntityKind, EntityAtom, PredicateAtom, FileURI, Start-End, PartialFingerprints, Fingerprints) :-
		to_atom(dead_predicate(EntityKind, EntityAtom, PredicateAtom), EntityPredicateFingerprint),
		to_atom(location(FileURI, Start-End), LocationFingerprint),
		to_atom(dead_predicate(EntityKind, EntityAtom, PredicateAtom, FileURI, Start-End), FindingFingerprint),
		PartialFingerprints = {
			entityPredicateV1-EntityPredicateFingerprint,
			locationV1-LocationFingerprint
		},
		Fingerprints = {
			canonicalFindingV1-FindingFingerprint
		}.

	sarif_message(EntityKind, Entity, Predicate, Message) :-
		atomic_list_concat(['Likely dead ', EntityKind, ' predicate ', Entity, '::', Predicate, '.'], Message).

	sarif_file_uri(File, URI) :-
		internal_os_path(File, Path),
		file_url_path(Path, URLPath),
		url_generate([scheme(file), authority(''), path(URLPath)], URI0),
		url_normalize(URI0, URI),
		!.

	predicate(Entity, Predicate, File, Lines, Options) :-
		non_scoped_predicate(Entity, Predicate0, File, Lines),
		% exclude coinduction user-defined hook predicates
		Predicate0 \== coinductive_success_hook/1,
		Predicate0 \== coinductive_success_hook/2,
		\+ used_by_scoped_predicate(Entity, Predicate0),
		% likely dead predicate found; check if it resulted
		% from the compilation of a non-terminal
		(	entity_property(Entity, defines(Predicate0, Properties)),
			member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Predicate0
		),
		\+ excluded_predicate(Predicate, Options).
	% unused predicates and non-terminals listed in the uses/2 directives
	predicate(Entity, Object::Resource, File, Start-End, Options) :-
		entity_property(Entity, calls(Object::Original, CallsProperties)),
		(	member(caller(Original), CallsProperties) ->
			Predicate = Original,
			entity_property(Entity, defines(Predicate, DefinesProperties))
		;	memberchk(alias(Alias), CallsProperties),
			memberchk(caller(Alias), CallsProperties),
			Predicate = Alias,
			entity_property(Entity, defines(Alias, DefinesProperties))
		),
		memberchk(auxiliary, DefinesProperties),
		memberchk(number_of_clauses(1), DefinesProperties),
		% Alias :- Object::Original linking clause that is generated when processing
		% uses/2 directives for allowing runtime use of listed resources
		\+ (
			entity_property(Entity, calls(Object::Original, OtherCallsProperties)),
			OtherCallsProperties \== CallsProperties
		),
		% no other callers for Object::Original
		\+ entity_property(Entity, calls(Predicate, _)),
		% no other callers for Original or Alias
		\+ entity_property(Entity, updates(Object::Original, _)),
		% not a predicate used as argument in calls to the database built-in methods
		\+ local_scope_directive(Entity, Predicate),
		\+ inherited_scope_directive(Entity, Predicate),
		% not a predicate (or non-terminal) made available (via a scope
		% directive) by the object containing the uses/2 directive
		(	memberchk(non_terminal(NonTerminal), CallsProperties),
			memberchk(non_terminal(NonTerminal), DefinesProperties) ->
			Resource = NonTerminal
		;	Resource = Predicate
		),
		(	member(include(File), CallsProperties) ->
			true
		;	entity_property(Entity, file(File))
		),
		memberchk(lines(Start, End), CallsProperties),
		\+ excluded_predicate(Object::Resource, Options).
	% unused predicates and non-terminals listed in the use_module/2 directives
	predicate(Entity, ':'(Module,Resource), File, Start-End, Options) :-
		entity_property(Entity, calls(':'(Module,Original), CallsProperties)),
		(	member(caller(Original), CallsProperties),
			Predicate = Original,
			entity_property(Entity, defines(Predicate, DefinesProperties))
		;	memberchk(alias(Alias), CallsProperties),
			memberchk(caller(Alias), CallsProperties),
			Predicate = Alias,
			entity_property(Entity, defines(Alias, DefinesProperties))
		),
		memberchk(auxiliary, DefinesProperties),
		memberchk(number_of_clauses(1), DefinesProperties),
		% Alias :- Module:Original linking clause that is generated when processing
		% use_module/2 directives for allowing runtime use of listed resources
		\+ (
			entity_property(Entity, calls(':'(Module,Original), OtherCallsProperties)),
			OtherCallsProperties \== CallsProperties
		),
		% no other callers for Module:Original
		\+ entity_property(Entity, calls(Predicate, _)),
		% no other callers for Original or Alias
		\+ entity_property(Entity, updates(':'(Module,Original), _)),
		% not a predicate used as argument in calls to the database built-in methods
		\+ local_scope_directive(Entity, Predicate),
		\+ inherited_scope_directive(Entity, Predicate),
		% not a predicate (or non-terminal) made available (via a scope
		% directive) by the object containing the use_module/2 directive
		(	memberchk(non_terminal(NonTerminal), CallsProperties),
			memberchk(non_terminal(NonTerminal), DefinesProperties) ->
			Resource = NonTerminal
		;	Resource = Predicate
		),
		(	member(include(File), CallsProperties) ->
			true
		;	entity_property(Entity, file(File))
		),
		memberchk(lines(Start, End), CallsProperties),
		\+ excluded_predicate(':'(Module,Resource), Options).

	non_scoped_predicate(Entity, Alias, File, Start-End) :-
		entity_property(Entity, defines(Alias, Properties)),
		Alias \= (_ :: _),
		% not a Logtalk multifile predicate definition
		Alias \= ':'(_, _),
		% not a Prolog multifile predicate definition
		\+ member(auxiliary, Properties),
		% not generated by the compiler or by the term-expansion mechanism
		(	entity_property(Entity, alias(Alias, AliasProperties)) ->
			memberchk(for(Predicate), AliasProperties)
		;	Predicate = Alias
		),
		\+ local_scope_directive(Entity, Predicate),
		% no local scope directive
		\+ inherited_scope_directive(Entity, Predicate),
		% no inherited scope directive
		(	current_category(Entity),
			complements_object(Entity, Object) ->
			non_scoped_predicate(Object, Predicate, _, _)
		;	true
		),
		% no scoped predicate in category complemented object
		(	member(include(File), Properties) ->
			true
		;	entity_property(Entity, file(File))
		),
		memberchk(lines(Start, End), Properties).

	inherited_scope_directive(Entity, Predicate) :-
		(	current_category(Entity) ->
			inherited_scope_directive(category, Entity, Predicate)
		;	% current_object(Entity),
			\+ instantiates_class(Entity, _),
			\+ specializes_class(Entity, _) ->
			inherited_scope_directive(prototype, Entity, Predicate)
		;	\+ instantiates_class(Entity, _) ->
			inherited_scope_directive(class, Entity, Predicate)
		;	\+ specializes_class(Entity, _) ->
			inherited_scope_directive(instance, Entity, Predicate)
		;	(	inherited_scope_directive(instance, Entity, Predicate)
			;	inherited_scope_directive(class, Entity, Predicate)
			)
		).

	inherited_scope_directive(EntityKind, Entity, Predicate) :-
		ancestor(EntityKind, Entity, AncestorKind, Ancestor),
		(	local_scope_directive(Ancestor, Predicate) ->
			true
		;	inherited_scope_directive(AncestorKind, Ancestor, Predicate)
		).

	% protocol ancestors
	ancestor(protocol, Entity, protocol, Ancestor) :-
		extends_protocol(Entity, Ancestor).
	% category ancestors
	ancestor(category, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(category, Entity, category, Ancestor) :-
		extends_category(Entity, Ancestor).
	% prototype ancestors
	ancestor(prototype, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(prototype, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(prototype, Entity, prototype, Ancestor) :-
		extends_object(Entity, Ancestor).
	% instance ancestors
	ancestor(instance, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(instance, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(instance, Entity, class, Ancestor) :-
		instantiates_class(Entity, Ancestor),
		Entity \== Ancestor.
	% class ancestors
	ancestor(class, Entity, protocol, Ancestor) :-
		implements_protocol(Entity, Ancestor).
	ancestor(class, Entity, category, Ancestor) :-
		imports_category(Entity, Ancestor).
	ancestor(class, Entity, class, Ancestor) :-
		specializes_class(Entity, Ancestor).

	local_scope_directive(Entity, Predicate) :-
		entity_property(Entity, public(Public)),
		member(Predicate, Public).
	local_scope_directive(Entity, Predicate) :-
		entity_property(Entity, protected(Protected)),
		member(Predicate, Protected).
	local_scope_directive(Entity, Predicate) :-
		entity_property(Entity, private(Private)),
		member(Predicate, Private).

	used_by_scoped_predicate(Entity, Predicate) :-
		entity_property(Entity, public(Public)),
		entity_property(Entity, protected(Protected)),
		entity_property(Entity, private(Private)),
		used_by_scoped_predicate(Predicate, Public, Protected, Private, [], Entity).

	% already inspected
	used_by_scoped_predicate(Predicate, _Public, _Protected, _Private, Tested, _Entity) :-
		member(Predicate, Tested),
		!,
		fail.
	% called by a scoped predicate
	used_by_scoped_predicate(Predicate, Public, Protected, Private, Tested, Entity) :-
		entity_property(Entity, calls(Predicate, Properties)),
		memberchk(caller(Caller), Properties),
		(	Caller = (_ :: _)
			% called from a Logtalk multifile predicate clause
		;	Caller = ':'(_, _)
			% called from a Prolog multifile predicate clause
		;	Caller == (:-)/1
			% called from an initialization/1 directive
		;	member(Caller, Public)
		;	member(Caller, Protected)
		;	member(Caller, Private)
		;	inherited_scope_directive(Entity, Caller)
		;	used_by_scoped_predicate(Caller, Public, Protected, Private, [Predicate| Tested], Entity)
		).

	entity_property(Entity, Property) :-
		(	current_object(Entity) ->
			object_property(Entity, Property)
		;	current_category(Entity) ->
			category_property(Entity, Property)
		;	atom(Entity), current_protocol(Entity) ->
			protocol_property(Entity, Property)
		;	% entity is not loaded
			fail
		).

	rlibrary(Library, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	expand_library_path(Library, TopPath) ->
			write_scan_header('Recursive library'),
			output_rlibrary(TopPath, Options),
			write_scan_footer('Recursive library')
		;	print_message(warning, dead_code_scanner, unknown(library,Library)),
			fail
		).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopPath, Options) :-
		^^option(exclude_libraries(ExcludedLibraries), Options),
		forall(
			(	sub_library(TopPath, Library, LibraryPath),
				\+ member(Library, ExcludedLibraries)
			),
			output_directory_files(LibraryPath, Options)
		).

	sub_library(TopPath, Library, LibraryPath) :-
		logtalk_library_path(Library, _),
		expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, _RelativePath, LibraryPath).

	library(Library, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	expand_library_path(Library, Path) ->
			write_scan_header('Library'),
			output_directory_files(Path, Options),
			write_scan_footer('Library')
		;	print_message(warning, dead_code_scanner, unknown(library,Library)),
			fail
		).

	library(Library) :-
		library(Library, []).

	rdirectory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	normalize_directory_path(Directory, Path),
			directory_exists(Path) ->
			write_scan_header('Recursive directory'),
			output_rdirectory(Path, Options),
			write_scan_footer('Recursive directory')
		;	print_message(warning, dead_code_scanner, unknown(directory,Directory)),
			fail
		).

	rdirectory(Directory) :-
		rdirectory(Directory, []).

	output_rdirectory(Directory, Options) :-
		(	setof(
				SubDirectory,
				sub_directory(Directory, SubDirectory, Options),
				SubDirectories
			) ->
			true
		;	SubDirectories = []
		),
		forall(
			member(SubDirectory, [Directory| SubDirectories]),
			output_directory_files(SubDirectory, Options)
		).

	sub_directory(Directory, SubDirectory, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		^^option(exclude_files(ExcludedFiles), Options),
		loaded_file_property(Path, basename(Basename)),
		not_excluded_file(ExcludedFiles, Path, Basename),
		decompose_file_name(Path, SubDirectory, _),
		Directory \== SubDirectory,
		sub_atom(SubDirectory, 0, _, _, Directory),
		\+ (
			member(ExcludedDirectory, ExcludedDirectories),
			sub_atom(SubDirectory, 0, _, _, ExcludedDirectory)
		).

	directory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	normalize_directory_path(Directory, Path),
			directory_exists(Path) ->
			write_scan_header('Directory'),
			output_directory_files(Path, Options),
			write_scan_footer('Directory')
		;	print_message(warning, dead_code_scanner, unknown(directory,Directory)),
			fail
		).

	directory(Directory) :-
		directory(Directory, []).

	output_directory_files(Directory, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		print_message(silent, dead_code_scanner, scanning_directory(Directory)),
		loaded_file_property(Path, directory(Directory)),
		loaded_file_property(Path, basename(Basename)),
		not_excluded_file(ExcludedFiles, Path, Basename),
		process_file(Path, Options),
		fail.
	output_directory_files(_, _).

	entity(Entity, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	current_object(Entity) ->
			Kind = object
		;	current_category(Entity) ->
			Kind = category
		;	current_protocol(Entity) ->
			Kind = protocol
		;	print_message(warning, dead_code_scanner, unknown(entity,Entity)),
			fail
		),
		write_scan_header('Entity'),
		process_entity(Kind, Entity, Options),
		write_scan_footer('Entity').

	entity(Entity) :-
		entity(Entity, []).

	process_entity(Kind, Entity, Options) :-
		print_message(silent, dead_code_scanner, scanning_entity(Kind, Entity)),
		Kind \== protocol,
		dead_code_finding(Kind, Entity, Finding, Options),
		print_message(warning, dead_code_scanner, Finding),
		fail.
	process_entity(_, _, _).

	target_entity(all, Kind, Entity, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		(	current_object(Entity),
			Kind = object
		;	current_category(Entity),
			Kind = category
		),
		\+ member(Entity, ExcludedEntities).

	target_entity(entity(Entity), Kind, Entity, _Options) :-
		(	current_object(Entity) ->
			Kind = object
		;	current_category(Entity) ->
			Kind = category
		;	fail
		).

	target_entity(file(Source), Kind, Entity, Options) :-
		locate_file(Source, Path),
		file_entity(Path, Kind, Entity, Options).

	target_entity(directory(Directory), Kind, Entity, Options) :-
		normalize_directory_path(Directory, Path),
		directory_exists(Path),
		directory_entity(Path, Kind, Entity, Options).

	target_entity(rdirectory(Directory), Kind, Entity, Options) :-
		normalize_directory_path(Directory, Path),
		directory_exists(Path),
		rdirectory_entity(Path, Kind, Entity, Options).

	target_entity(library(Library), Kind, Entity, Options) :-
		expand_library_path(Library, Path),
		directory_entity(Path, Kind, Entity, Options).

	target_entity(rlibrary(Library), Kind, Entity, Options) :-
		expand_library_path(Library, TopPath),
		rlibrary_entity(TopPath, Kind, Entity, Options).

	file(Source, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		locate_file(Source, Path),
		write_scan_header('File'),
		process_file(Path, Options),
		write_scan_footer('File').

	file(Source) :-
		file(Source, []).

	% file given in library notation
	locate_file(LibraryNotation, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Path).
	% file given using its name or basename
	locate_file(Source, Path) :-
		add_extension(Source, Basename),
		loaded_file_property(Path, basename(Basename)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		!.
	% file given using a full path
	locate_file(Source, Path) :-
		add_extension(Source, SourceWithExtension),
		loaded_file_property(Path, basename(Basename)),
		loaded_file_property(Path, directory(Directory)),
		atom_concat(Directory, Basename, SourceWithExtension),
		!.

	add_extension(Source, SourceWithExtension) :-
		% ensure that Source is not specified using library notation
		atom(Source),
		decompose_file_name(Source, _, _, SourceExtension),
		(	file_type_extension(source, SourceExtension) ->
			% source file extension present
			SourceWithExtension = Source
		;	% try possible source extensions
			file_type_extension(source, Extension),
			atom_concat(Source, Extension, SourceWithExtension)
		).

	process_file(Path, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		print_message(silent, dead_code_scanner, scanning_file(Path)),
		file_entity(Path, Kind, Entity, Options),
		\+ member(Entity, ExcludedEntities),
		process_entity(Kind, Entity, Options),
		fail.
	process_file(_, _).

	file_entity(Path, Kind, Entity, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		(	loaded_file_property(Path, object(Entity)),
			Kind = object
		;	loaded_file_property(Path, category(Entity)),
			Kind = category
		),
		\+ member(Entity, ExcludedEntities).

	directory_entity(Directory, Kind, Entity, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		loaded_file_property(Path, directory(Directory)),
		loaded_file_property(Path, basename(Basename)),
		not_excluded_file(ExcludedFiles, Path, Basename),
		file_entity(Path, Kind, Entity, Options).

	rdirectory_entity(Directory, Kind, Entity, Options) :-
		(	SubDirectory = Directory
		;	sub_directory(Directory, SubDirectory, Options)
		),
		directory_entity(SubDirectory, Kind, Entity, Options).

	rlibrary_entity(TopPath, Kind, Entity, Options) :-
		^^option(exclude_libraries(ExcludedLibraries), Options),
		sub_library(TopPath, Library, LibraryPath),
		\+ member(Library, ExcludedLibraries),
		directory_entity(LibraryPath, Kind, Entity, Options).

	all(UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(exclude_entities(ExcludedEntities), Options),
		(	current_object(Entity),
			Kind = object
		;	current_category(Entity),
			Kind = category
		),
		\+ member(Entity, ExcludedEntities),
		process_entity(Kind, Entity, Options),
		fail.
	all(_).

	all :-
		write_scan_header('All entities'),
		all([]),
		write_scan_footer('All entities').

	write_scan_header(Type) :-
		print_message(silent, dead_code_scanner, scan_started),
		date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(silent, dead_code_scanner, scan_start_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)),
		print_message(comment, dead_code_scanner, scanning_for_dead_code).

	write_scan_footer(Type) :-
		print_message(comment, dead_code_scanner, completed_scanning_for_dead_code),
		date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(silent, dead_code_scanner, scan_end_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)),
		print_message(silent, dead_code_scanner, scan_ended).

	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, don't exclude any predicates:
	default_option(exclude_predicates([])).
	% by default, don't waive any findings:
	default_option(waive_findings([])).
	% by default, don't validate exported JSON at runtime:
	default_option(validate_export(false)).
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).

	valid_option(exclude_directories(Directories)) :-
		valid(list(atom), Directories).
	valid_option(exclude_files(Files)) :-
		valid(list(atom), Files).
	valid_option(exclude_entities(Entities)) :-
		valid(list(atom), Entities).
	valid_option(exclude_predicates(Predicates)) :-
		valid(list(compound), Predicates).
	valid_option(waive_findings(Findings)) :-
		valid(list(compound), Findings).
	valid_option(validate_export(true)).
	valid_option(validate_export(false)).
	valid_option(exclude_libraries(Libraries)) :-
		valid(list(atom), Libraries).

	excluded_predicate(Predicate, Options) :-
		^^option(exclude_predicates(ExcludedPredicates), Options),
		memberchk(Predicate, ExcludedPredicates).

	waived_finding(Finding, Options) :-
		^^option(waive_findings(WaivedFindings), Options),
		\+ \+ member(Finding, WaivedFindings).

	not_excluded_file([], _, _).
	not_excluded_file([ExcludedFile| ExcludedFiles], Path, Basename) :-
		% files in the exclusion list may be given by full path or by basename
		\+ member(Path, [ExcludedFile| ExcludedFiles]),
		\+ member(Basename, [ExcludedFile| ExcludedFiles]),
		% files in the exclusion list may be given with or without extension
		\+ (	file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Path),
				member(Source, [ExcludedFile| ExcludedFiles])
		),
		\+ (	file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Basename),
				member(Source, [ExcludedFile| ExcludedFiles])
		).

	fix_option(exclude_directories(Directories0), exclude_directories(Directories)) :-
		normalize_directory_paths(Directories0, Directories).

	normalize_directory_paths([], []).
	normalize_directory_paths([Directory0| Directories0], [Directory| Directories]) :-
		normalize_directory_path(Directory0, Directory),
		normalize_directory_paths(Directories0, Directories).

	normalize_directory_path(Directory0, Directory) :-
		internal_os_path(Directory1, Directory0),
		absolute_file_name(Directory1, Directory2),
		(	sub_atom(Directory2, _, _, 0, '/') ->
			Directory = Directory2
		;	atom_concat(Directory2, '/', Directory)
		).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
:- endif.
