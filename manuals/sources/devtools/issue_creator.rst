``issue_creator``
=================

This is a complementary tool for the ``lgtunit`` tool for automatically
creating bug report issues for failed tests in GitHub or GitLab servers.

Requirements
------------

This tool requires that the GitHub and GitLab CLIs be installed. For the
installation instructions see:

-  GitHub: https://cli.github.com
-  GitLab: https://glab.readthedocs.io

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(issue_creator(loader)).

Usage
-----

To use this tool, simply load it and if necessary define the
``issue_server`` Logtalk flag. The possible values for this flag are the
atoms ``github`` (the default value) and ``gitlab``. The
``logtalk_tester`` automation script accepts a ``-b`` option for
automatically use this tool (see the script man page for details). For
example:

::

   $ logtalk_tester \
       -p gnu \
       -b github \
       -s "/home/jdoe/foo/" \
       -u https://github.com/jdoe/foo/tree/55aa900775befa135e0d5b48ea63098df8b97f5c/

In this case, the script **must** be called from a git repo directory or
one of its sub-directories, which is a common setup in CI/CD pipelines.
Moreover, prior to running the tests, the CLI must be used to
authenticate and login to the server where the bug report issues will be
created:

-  GitHub: ``gh auth login``
-  GitLab: ``glab auth login``

See the CLIs documentation for details. Typically, the ``auth`` command
is called from the CI/CD pipeline definition scripts.

The bug reports are created using by default the label ``bug`` and
assigned to the author of the latest commit of the git repo. The ``-b``
option can be used to override the label with a comma separated set of
labels. For example, to use both ``bug`` and ``auto`` labels:

::

   $ logtalk_tester \
       -p gnu \
       -b github:bug,auto \
       -s "/home/jdoe/foo/" \
       -u https://github.com/jdoe/foo/tree/55aa900775befa135e0d5b48ea63098df8b97f5c/

Note that the labels **must** be predefined in the issue tracker server
for the bug report to be successfully created.

Known issues
------------

This tool is in an early stage of development and changes are to be
expected. Your feedback is most appreciated.
