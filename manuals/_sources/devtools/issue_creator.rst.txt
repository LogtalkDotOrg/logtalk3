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
atoms ``github`` (the default value) and ``gitlab``. Prior to running
the tests, the CLI must be used to authenticate and login to the server
where the bug report issues will be created:

-  GitHub: ``gh auth login``
-  GitLab: ``glab auth login``

See the CLIs documentation for details.

Known issues
------------

This tool is in an early stage of development and changes are thus to be
expected. Your feedback is most appreciated.
