Overview
========

The following developer tools are available, each one with its own
``loader.lgt`` loader file (except for the built-in ``linter`` and
``make`` tools, which are integrated with the compiler/runtime) and
``NOTES.md`` documentation files:

-  ``asdf``
-  ``assertions``
-  ``code_metrics``
-  ``dead_code_scanner``
-  ``debug_messages``
-  ``debugger``
-  ``diagrams``
-  ``doclet``
-  ``help``
-  ``issue_creator``
-  ``lgtdoc``
-  ``lgtunit``
-  ``linter``
-  ``make``
-  ``packs``
-  ``ports_profiler``
-  ``profiler``
-  ``tutor``
-  ``wrapper``

Loading the developer tools
---------------------------

To load the main developer tools, use the following goal:

::

   | ?- logtalk_load(tools(loader)).

The ``ports_profiler`` tool is not loaded by default, however, as it
conflicts with the ``debugger`` tool as both provide a debug handler
that must be unique in a running session.

The ``profiler`` tool is also not loaded by default as it provides
integration with selected backend Prolog compiler profilers that are not
portable.

The ``tutor`` tool is also not loaded by default given its useful mainly
for new users that need help understanding compiler warning and error
messages.

The ``wrapper`` tool is also not loaded by default given its specialized
purpose and beta status.

To load a specific tool, either change your Prolog working directory to
the tool folder and then compile and load the corresponding loader
utility file or simply use library notation as argument for the
compiling and loading predicates. For example:

::

   | ?- logtalk_load(lgtunit(loader)).

Tools documentation
-------------------

Specific notes about each tool can be found in the corresponding
``NOTES.md`` files. HTML documentation for each tool API can be found on
the ``docs`` directory (open the ``../docs/index.html`` file with your
web browser). The documentation for these tools can be regenerated using
the shell scripts ``../scripts/update_html_docs.sh`` and
``../scripts/update_svg_diagrams.sh``.

Tools common flags
------------------

The ``lgtdoc`` and ``lgtunit`` tools share a ``suppress_path_prefix``
flag that can be used to suppress a prefix when printing file paths. For
example (after loading the tools):

::

   | ?- set_logtalk_flag(suppress_path_prefix, '/home/jdoe/').

Tools requirements
------------------

Some of the developer tools have third-party dependencies. For example,
the ``lgtdoc`` tool depends on XSLT processors to generate documentation
final formats and uses Sphinx for the preferred HTML final format. Be
sure to consult the tools documentation details on those requirements
and possible alternatives. For convenience, follows a global list of the
main tool requirements and suggestions for installing them per
operating-system. If your operating-system or a dependency for it is not
listed, see the dependency websites for installation instructions.

Tool dependencies for full functionality
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  ``diagrams``: Graphviz
-  ``help``: ``info``
-  ``issue_creator``: ``gh``, ``glab``
-  ``lgtdoc``: Sphinx, ``libxslt``, ``fop``, ``texlive``, ``texinfo``
-  ``lgtunit``: Allure
-  ``packs``: ``coreutils``, ``libarchive``, ``gnupg2``, ``git``,
   ``curl``, ``direnv``

Python dependencies (all operating-systems)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   $ pip install --upgrade pygments
   $ pip install --upgrade sphinx
   $ pip install --upgrade sphinx_rtd_theme

macOS - MacPorts
~~~~~~~~~~~~~~~~

::

   $ sudo port install graphviz
   $ sudo port install texinfo
   $ sudo port install gh glab
   $ sudo port install libxslt fop texlive
   $ sudo port install coreutils libarchive gnupg2 git curl direnv

macOS - Homebrew
~~~~~~~~~~~~~~~~

::

   $ brew install graphviz
   $ brew install texinfo
   $ brew install libxslt fop texlive
   $ brew install allure
   $ brew install coreutils libarchive gnupg2 git curl direnv
   $ brew install gh glab

Ubuntu
~~~~~~

::

   $ sudo apt install graphviz
   $ sudo apt install info
   $ sudo apt install xsltproc fop texlive
   $ sudo apt-add-repository ppa:qameta/allure && sudo apt install allure
   $ sudo apt install libarchive-tools gnupg2 git curl direnv

RedHat
~~~~~~

::

   $ sudo dnf install graphviz
   $ sudo dnf install libxslt fop
   $ sudo dnf install bsdtar gnupg2 git curl direnv

Windows - Chocolatey
~~~~~~~~~~~~~~~~~~~~

::

   > choco install graphviz
   > choco install xsltproc apache-fop texlive
   > choco install gnupg git
   > choco install gh glab

Windows - installers
~~~~~~~~~~~~~~~~~~~~

| https://www.graphviz.org/download/
| https://docs.qameta.io/allure-report/
| https://www.gnupg.org/
| https://gitforwindows.org
| https://cli.github.com
| https://glab.readthedocs.io

Windows - PowerShell add-ons
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   PS> Install-Module -Name Set-PsEnv
