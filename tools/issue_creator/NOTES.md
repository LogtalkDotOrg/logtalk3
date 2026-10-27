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


`issue_creator`
===============

This is a complementary tool for the `lgtunit` tool for automatically
creating bug reports for failed tests in Azure DevOps, Gitea, GitHub,
GitLab, Jira, or Linear issue tracker servers.


Requirements
------------

This tool requires that the Azure `az`, Gitea `tea`, GitHub `gh`, GitLab
`glab`, Jira `jira`, or Linear `linear` CLIs be installed (depending on
the issue tracker used). For the installation instructions see:

- Azure DevOps: https://learn.microsoft.com/en-us/cli/azure/install-azure-cli
- Gitea: https://gitea.com/gitea/tea
- GitHub: https://cli.github.com
- GitLab: https://glab.readthedocs.io
- Jira: https://github.com/ankitpokhrel/jira-cli
- Linear: https://github.com/schpet/linear-cli

For Azure DevOps, the `az` CLI must also have the `azure-devops` extension
installed:

	$ az extension add --name azure-devops


Loading
-------

To load this tool, load its `loader.lgt` file:

	| ?- logtalk_load(issue_creator(loader)).

But, in the most common usage scenario, this tool is automatically loaded by
the `logtalk_tester` automation script.


Testing
-------

To test this tool, load its `tester.lgt` file:

	| ?- logtalk_load(issue_creator(tester)).

The tests cover failure descriptions, tracker-specific formatting, title
identity, search and creation arguments, existence commands, source links,
report content, flaky-test filtering, and server selection. Harmless local
commands check argument preservation and command outcomes. The test goals do
not execute tracker commands or require issue tracker CLIs, credentials, or
access to an issue tracker server. Git metadata collection, live searches,
assignment retries, and actual issue creation are not tested.

When the tool is not already loaded, the test runner uses a compilation hook
to omit its reporting callback, allowing the tests to run offline. When the
`logtalk_tester` script loads the tool using its `-b` option, the test runner
preserves the loaded reporter and its configuration. Temporary flag values
used by individual tests are restored before test results are reported,
including when a test goal fails or throws an exception. Automatic reporting
retains its normal CLI, authentication, and server requirements.


Usage
-----

The `logtalk_tester` automation script accepts a `-b` option for automatically
using this tool (see the script man page for details). As custom labels are
optional, in the most simple case this option specifies only the issue tracker
(possible values are `azure`, `gitea`, `github`, `gitlab`, `jira`, and
`linear`). For example:

	$ logtalk_tester \
		-p gnu \
		-b github \
		-s "/home/jdoe/foo/" \
		-u https://github.com/jdoe/foo/tree/55aa900775befa135e0d5b48ea63098df8b97f5c/

The `-u` option is used by the `logtalk_tester` automation scripts to set the
`tests_base_url` flag value. This tool uses this flag to construct working
test-file links by appending the test file path and line fragment to the `-u`
option value (except for Azure DevOps; see notes below).

The `logtalk_tester` script **must** be called from a git repo directory or one
of its sub-directories, which is a common setup in CI/CD pipelines. Moreover,
prior to running the tests, the CLI must be used, if required, to authenticate
and login to the server where the bug reports will be created:

- Azure DevOps: `az login` or `echo $TOKEN | az devops login --organization <url>`
  (alternatively, set the `AZURE_DEVOPS_EXT_PAT` environment variable)
- Gitea: `tea login add --name <string> --url <string> --token <string>`
- GitHub: `gh auth login --hostname <string> --with-token < token.txt`
- GitLab: `glab auth login --hostname <string> --token <string>`
- Jira: set the `JIRA_API_TOKEN` environment variable and run `jira init`
  (see the `jira-cli` documentation for the other authentication types)
- Linear: see the `linear-cli` documentation (the credential can be selected
  using the `linear` global option `--workspace`)

The access token must have the necessary scopes that allow bug reports to be
created. See the CLIs documentation for details. Typically, the `auth` command
is called from the CI/CD pipeline definition scripts. However, depending on the
CI/CD workflow, the authentication may be done implicitly.

The bug reports are created using by default the label `bug` and assigned to
the author of the latest commit of the git repo. The `-b` option can also be
used to override the default label with a comma separated set of labels. The
syntax is `-b SERVER:LABELS`. For every server, labels must be a
comma-separated list of nonempty names. Each name must start with an ASCII
letter or digit and contain only ASCII letters, digits, underscores (`_`),
or hyphens (`-`). Spaces, quotes, and other characters are not supported.
The scripts use this option to set the `issue_labels` flag. Invalid labels
suppress automatic reporting before any Git metadata or tracker command is run.
For example, to use both `bug` and `auto` labels:

	$ logtalk_tester \
		-p gnu \
		-b github:bug,auto \
		-s "/home/jdoe/foo/" \
		-u https://github.com/jdoe/foo/tree/55aa900775befa135e0d5b48ea63098df8b97f5c/

Note that, except for Jira, the labels **must** be predefined in the issue
tracker for the bug report to be successfully created. The `auto` label can
be used to filter auto-generated bug reports when browsing the issue tracker.

The bug reports use Markdown formatting, which is the default in GitHub and
GitLab issue trackers.

Reports are only created for non-flaky tests, honoring both the `flaky` test
option and notes containing `flaky`. The reported execution time is CPU time
in seconds, not wall time. The bug report title and labels are used to prevent
creating duplicated bug reports. Therefore, the title should not be manually
edited and the same labels should be used for multiple runs of the same tests
and preserved when editing the bug reports. Titles identify the test name and
the test set path after suppressing the configured prefix. For example:

	Test invoice_creation of test set library/invoices/ failed

GitHub, GitLab, Gitea, and Linear duplicate checks search for this title and
the configured labels. For other issue trackers, see the notes below.


Azure DevOps notes
------------------

For Azure DevOps (`-b azure`), the organization and project are detected from
the git remote when possible. Otherwise, set them as defaults using:

	$ az devops configure --defaults organization=<url> project=<name>

Bug reports are created as work items of type `Bug` (using the `Repro Steps`
field for the description, which is HTML in Azure DevOps) and the labels are
set as work item tags. The tool attempts to assign the work item to the author
of the latest commit using the author email. If the creation command fails
with a nonempty assignee, the tool retries without an assignee, regardless of
the cause of failure. The retry can also fail; unassigned creation is not
guaranteed. The base URL (`-u` option) should be the Azure Repos repository
URL, optionally including the version, to allow generating links to the test
files. The tool appends `path` and `line` query parameters instead of a file
path and line fragment. The `path` value is the test file path after
suppressing the configured prefix, with a leading `/` added when needed. The
`line` value is the first line of the reported test position. The parameters
are introduced by `?` or, if the base URL already contains a query, by `&`.
For example:

	$ logtalk_tester \
		-p gnu \
		-b azure:bug,auto \
		-s "/home/jdoe/foo/" \
		-u "https://dev.azure.com/jdoe/foo/_git/foo?version=GC55aa900775befa135e0d5b48ea63098df8b97f5c"

Duplicated bug reports are detected using a WIQL query for work items of type
`Bug`, in the selected project, with the same title and tags. The tool reads
the project's Bug workflow using `az devops invoke` and queries the actual
state names whose categories are neither `Completed` nor `Removed`. This
supports custom workflow state names and requires permission to read project
and work item type metadata, in addition to querying and creating work items.
A project lookup, workflow lookup, or issue query failure suppresses creation.
An empty or invalid workflow response also suppresses creation. This requires
a process template where the `Bug` work item type exists (i.e. Agile, Scrum,
or CMMI; not Basic).

On Windows, the Azure duplicate check requires `powershell` on the executable
search path. The query program is passed as a UTF-16LE Base64 encoded command
to avoid `cmd.exe` expansion. Offline tests verify command construction and
encoding; execution on Windows and against a live Azure server is not tested.


Gitea notes
-----------

For Gitea (`-b gitea`), the `tea` CLI detects the repository and login from
the git remote of the current directory, so a matching login must have been
added using `tea login add`. The tool attempts to assign bug reports to the
author of the latest commit using the local part of the author email as the
Gitea user name. If the creation command fails with a nonempty assignee, the
tool retries without an assignee, regardless of the cause of failure. The
retry can also fail; unassigned creation is not guaranteed. The base URL
(`-u` option) should be the commit URL for the repository. For example:

	$ logtalk_tester \
		-p gnu \
		-b gitea:bug,auto \
		-s "/home/jdoe/foo/" \
		-u https://gitea.example.com/jdoe/foo/src/commit/55aa900775befa135e0d5b48ea63098df8b97f5c/


Jira notes
-----------

For Jira (`-b jira`), the `jira` CLI must have been configured (using
`jira init`) with the server, the project, and the authentication type. Bug
reports are created as issues of type `Bug` in the configured project (so
that type must exist and the project must not require other fields to be
set when creating issues), with the labels set as Jira labels (which, unlike
in GitHub and GitLab, don't need to be predefined). The tool attempts to assign
the issue to the author of the latest commit using the author email. If the
creation command fails with a nonempty assignee, the tool retries without an
assignee, regardless of the cause of failure. The retry can also fail;
unassigned creation is not guaranteed. Jira is not a code host, so the base
URL (`-u` option) is the URL of the server hosting the repository, e.g.
GitHub, GitLab, or Gitea, in order to generate links to the test files.
Duplicated bug reports are detected using a JQL query that looks for issues
of type `Bug`, not in the `Done` status category, with the same labels and
a matching summary. For example:

	$ logtalk_tester \
		-p gnu \
		-b jira:bug,auto \
		-s "/home/jdoe/foo/" \
		-u https://git.example.com/jdoe/foo/src/commit/55aa900775befa135e0d5b48ea63098df8b97f5c/


Linear notes
------------

For Linear (`-b linear`), the `linear` CLI must be authenticated and have a
default team configured (the team is not selected by this tool). Bug reports
are created in that team using the default workflow state and template, with
the labels set as Linear labels (which must exist). The tool attempts to assign
the issue to the author of the latest commit using the author email. If the
creation command fails with a nonempty assignee, the tool retries without an
assignee, regardless of the cause of failure. The retry can also fail;
unassigned creation is not guaranteed. Linear descriptions are Markdown.
As for Jira, the base URL (`-u` option) is the URL of the server hosting
the repository. Duplicated bug reports are detected by searching for the
constructed title in team issues, assigned to anyone, in the triage, backlog,
unstarted, or started state with the same labels. The query requests at most
one match. For example:

	$ logtalk_tester \
		-p gnu \
		-b linear:bug,auto \
		-s "/home/jdoe/foo/" \
		-u https://git.example.com/jdoe/foo/src/commit/55aa900775befa135e0d5b48ea63098df8b97f5c/


Suspending creation of bug reports
----------------------------------

There are cases where we may want to postpone or temporarily disable the
automatic creation of bug reports. E.g. a WIP branch that's known to break
multiple tests. A solution is to define a pull/merge request label, e.g.
`NO_AUTO_BUG_REPORTS`, that can then be checked by the CI/CD workflow. For
example, we can test the presence of that label to set a `AUTO_BUG_REPORTS`
environment variable to either an empty string or a `-b` option and use:

	logtalk_tester.sh $AUTO_BUG_REPORTS -p ...


Known issues
------------

GitLab creates CI/CD pipelines in a detached HEAD state by default. As a
consequence, the git branch would be reported as HEAD. To workaround this
issue, this tool uses the value of the GitLab CI/CD pipeline variable
`CI_COMMIT_REF_NAME` when defined as the branch name.

This tool is in a beta stage of development. Your feedback is most appreciated.
