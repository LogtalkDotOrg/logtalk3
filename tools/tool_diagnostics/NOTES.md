________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`tool_diagnostics`
==================

This tool provides the shared `tool_diagnostics_protocol` protocol and the
`tool_diagnostics_common` category used by developer tools such as
`dead_code_scanner`, `lgtdoc`, `lgtunit`, and `linter_reporter` to expose
machine-readable diagnostics.

The protocol is designed so that producer tools can keep their own internal
semantics while still providing a common interface for querying diagnostics
directly or for generating SARIF reports using the `sarif` tool.


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#tool_diagnostics_protocol](../../apis/library_index.html#tool_diagnostics_protocol)


Loading
-------

Load the protocol and common category definitions using:

	| ?- logtalk_load(tool_diagnostics(loader)).
    ...


Protocol overview
-----------------

The protocol defines predicates for reporting:

- tool metadata using `diagnostics_tool/5`
- supported target patterns using `diagnostic_target/1`
- rule descriptors using `diagnostic_rule/5` and `diagnostic_rules/1`
- diagnostics using `diagnostic/2-3` and `diagnostics/2-3`
- summaries using `diagnostics_summary/2-3`
- preflight issues using `diagnostics_preflight/2-3`

Common target terms include `all`, `file(File)`,
`directory(Directory)`, `rdirectory(Directory)`, `library(Library)`,
`rlibrary(Library)`, `entity(Entity)`, and `tests(Object)`.

Predicates taking an `Options` argument must accept the common option
`explanations(Boolean)`. Tools may use it to include `explanation(Text)`
properties when available, but implementations that do not provide additional
explanation text must still accept the option as a no-op.


Term conventions
----------------

Tool metadata uses terms of the form:

	diagnostics_tool(Id, Name, Version, InformationURI, Properties)

The `Id` and `Name` arguments identify the tool. The `Version` argument
should be derived reflectively from the implementing object `info/1` metadata
using `object_property/2` instead of being hardcoded. The `InformationURI`
argument is a stable tool home or documentation URI and is exported by the
`sarif` tool as the SARIF `tool.driver.informationUri` value.

The `Properties` list is intended for exporter and post-processing hints. The
current `sarif` tool interprets the following entries:

- `guid(Guid)` sets the SARIF `tool.driver.guid` value when present.
- `fingerprint_algorithm(Algorithm)` sets the exported run
	`fingerprintAlgorithm` property and selects the canonical result
	fingerprint key. The current implementation distinguishes
	`canonical_finding_v1` from the default warning-oriented path and
	computes result identities from normalized relative locations instead
	of raw absolute file paths.
- `automation_id(target)` requests that the SARIF
	`run.automationDetails.id` value be derived from the queried target term,
	allowing different targets of the same tool to produce distinct run
	identities.
- `automation_id(tool)` denotes a tool-scoped automation identifier. In the
	current `sarif` implementation this is redundant because omitting
	`automation_id(target)` already falls back to the tool identifier.
- `include_invocations(true)` requests a SARIF `invocations` section,
	including `toolExecutionNotifications` when preflight issues exist.
- `include_git_metadata(true)` requests SARIF `gitBranch` and
	`gitCommitHash` run properties when a stable Git context can be inferred.
	When this succeeds, the `sarif` tool also normalizes artifact locations and
	fingerprint inputs relative to the repository root. The `gitBranch` value is
	descriptive metadata; the `gitCommitHash` value provides the stable revision
	identity used to disambiguate repository-relative paths.
- `include_version_control_provenance(true)` requests SARIF
	`versionControlProvenance` data when repository details can be inferred
	from diagnostic file paths.
- `count_key(Key)` selects the property name used for the exported count,
	e.g. `diagnosticsCount` or `totalWarnings`.

When `include_git_metadata(true)` is not present or a stable Git context
cannot be inferred, the `sarif` tool falls back to application-root-relative
artifact locations and fingerprint inputs instead.

Rule descriptors use terms of the form:

	diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties)

The `diagnostic_rules/1` predicate should return these descriptors in a stable
order. Rule descriptor properties may contain terms such as `guid(Guid)`,
`help(Text)`, `help_uri(URI)`, `tags(Tags)`, and `precision(Precision)`.

Diagnostics use terms of the form:

	diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties)

The `Context` argument is represented using the term `context(Kind,
Identifier)` where `Kind` is a tool-defined atom such as `file`, `object`,
`category`, `protocol`, or `test_set`. For file-scoped diagnostics,
`Identifier` is the source file path when available and the empty atom
otherwise. Diagnostics originating in an included file may still use an entity
context when the `include/1` directive appears inside that entity; otherwise
they are file-scoped.

The `File` argument is a source file path or the empty atom when unavailable.
The `Lines` argument uses the `Start-End` notation. The value `0-0` means that
no precise line information is available.

Supported severity values are `error`, `warning`, and `note`. Supported
confidence values are `high`, `medium`, `low`, and `not_applicable`.

The `Properties` argument should include stable structured metadata such as
`raw_term(Term)`, `explanation(Text)`, `flag(Flag)`,
`directive(Directive)`, `indicator(Indicator)`, `key(Key)`, `test(Test)`,
`option(Option)`, and any other tool-specific facts useful for
post-processing.

Preflight issues use terms of the form:

	preflight_issue(Id, Severity, Message, Context, File, Lines, Properties)

Preflight issues describe prerequisites or analysis quality issues instead of
findings.

Summaries use terms of the form:

	diagnostics_summary(Target, TotalContexts, TotalDiagnostics, Breakdown, ContextSummaries)

The `Breakdown` argument uses a
`diagnostic_breakdown(RuleCounts, SeverityCounts, ConfidenceCounts)` term and
`ContextSummaries` is a list of
`context_summary(Context, DiagnosticsCount, Breakdown)` terms.

Rule counts use `rule_count(RuleId, Count)` terms, severity counts use
`severity_count(Severity, Count)` terms, and confidence counts use
`confidence_count(Confidence, Count)` terms.


Usage
-----

Tools implementing this protocol can be queried directly for diagnostics after
compiling code that results in the production of diagnostics. For example:

	| ?- logtalk_load(dead_code_scanner(loader)).
    ...

    | ?- logtalk_load(my_library(loader)).
    ...

	| ?- dead_code_scanner::diagnostics(library(my_library), Diagnostics).
    ...

To serialize diagnostics as SARIF, load the standalone `sarif` tool and
generate a report from a diagnostics producer:

	| ?- logtalk_load(sarif(loader)).
    ...

	| ?- sarif::generate(dead_code_scanner, entity(my_object), file('./report.sarif'), []).
    true.

To generate an explicit aggregate SARIF report with multiple diagnostics
producers, pass a list of `tool_spec(Tool, Target, Options)` terms:

	| ?- sarif::generate([
	|        tool_spec(linter_reporter, all, []),
	|        tool_spec(dead_code_scanner, entity(my_object), [])
	|    ], file('./aggregate.sarif')).
    true.
