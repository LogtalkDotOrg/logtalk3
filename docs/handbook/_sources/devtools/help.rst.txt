.. _library_help:

``help``
========

This tool provides help for Logtalk features and libraries when running
in most operating-systems. For help on the Logtalk compiler error and
warning messages, see the ``tutor`` tool.

Requirements
------------

On Windows, the ``start`` command must be available. On Linux, the
``xdg-open`` command must be available. On macOS, the command ``open``
is used.

Browsing the Handbook and APIs documentation at the top-level requires a
POSIX system and one of the following terminal-based browsers installed:

- ```lynx`` <https://invisible-island.net/lynx/>`__
- ```w3m`` <https://w3m.sourceforge.net/>`__
- ```links`` <http://links.twibright.com/>`__

On Windows systems, the documentation is open in the default browser.

API documentation
-----------------

This tool API documentation is available at:

`../../apis/library_index.html#help <../../apis/library_index.html#help>`__

For sample queries, please see the ``SCRIPT.txt`` file in the tool
directory.

Loading
-------

::

   | ?- logtalk_load(help(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(help(tester)).

Supported operating-systems
---------------------------

Currently, support is limited to Linux, macOS, and Windows.

This tool relies on the library portable operating-system access
abstraction.

Usage
-----

After loading the tool, use the query ``help::help`` to get started.
Some query examples:

::

   | ?- help::handbook.

   | ?- help::handbook(base64).

   | ?- help::handbook(logtalk_load/2).

   | ?- help::apis.

   | ?- help::apis(check/2).

   | ?- help::apis(message_tokens//2).

Although less useful, you can also browse the ``man`` pages of Logtalk
scripts. For example:

::

   | ?- help::man(logtalk_tester).

When using the terminal-based browsers, after finishing consulting the
documentation and quitting the process, you will be back to the
top-level prompt (if you find that the top-level have scrolled from its
last position, try to set your terminal terminfo to
``xterm-256colour``).

If you're running Logtalk from a git clone of its repo, you will need to
run the ``docs/apis/sources/build.sh`` or
``docs/apis/sources/build.ps1`` scripts to generate APIs documentation
HTML files and also run the ``docs/handbook/sources/build.sh`` or
``docs/handbook/sources/build.ps1`` scripts to generate the Handbook
HTML files. Alternatively, you can download the documentation for the
latest stable release from the Logtalk website and save them to the
``docs`` directories.

On POSIX systems, one of the supported terminal-based browsers must be
installed unless you prefer using the default browser. The tool checks
first for ``lynx``, second for ``w3m``, and finally for ``links``.

On macOS, these browsers can be installed with either MacPorts:

::

   $ sudo port install lynx
   $ sudo port install w3m
   $ sudo port install links

Or using Homebrew:

::

   $ brew install lynx
   $ brew install w3m
   $ brew install links

On Linux systems, use the distribution's own package manager to install
the browsers. For example, in Ubuntu systems:

::

   $ sudo apt install lynx
   $ sudo apt install w3m

Known issues
------------

When using the terminal-based browsers, the Handbook and APIs search
boxes are not usable as they require JavaScript support. Use instead the
indexes.

The open commands used to open documentation URLs in the default browser
drop the fragment part, thus preventing navigation to the specified
position on the documentation page.

ECLiPSe defines a ``help`` prefix operator that forces wrapping this
atom between parentheses when sending messages to the tool. E.g. use
``(help)::help`` instead of ``help::help``.
