.. _library_mcp_server:

``mcp_server``
==============

MCP (Model Context Protocol) server library for Logtalk applications.
Makes any Logtalk application available as a local MCP server using
stdio transport with Content-Length framing. Implements the MCP
2025-03-26 specification (tools and elicitation capabilities).

The library uses the ``json_rpc`` library for JSON-RPC 2.0 message
handling as currently required by the MCP specification.

API documentation
-----------------

Open the
`../../apis/library_index.html#mcp_server <../../apis/library_index.html#mcp_server>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(mcp_server(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(mcp_server(tester)).

Usage
-----

Implementing the tool protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To expose a Logtalk object as an MCP tool provider, implement the
``mcp_tool_protocol`` protocol:

.. code:: logtalk

   :- object(my_tools,
       implements(mcp_tool_protocol)).

       :- public(factorial/2).
       :- mode(factorial(+integer, -integer), one).
       :- info(factorial/2, [
           comment is 'Computes the factorial of a non-negative integer.',
           argnames is ['N', 'F']
       ]).

       tools([
           tool(factorial, factorial, 2)
       ]).

       factorial(0, 1) :- !.
       factorial(N, F) :-
           N > 0,
           N1 is N - 1,
           factorial(N1, F1),
           F is N * F1.

   :- end_object.

The ``tools/1`` predicate returns a list of
``tool(Name, Functor, Arity)`` descriptors that declare which predicates
are exposed as MCP tools:

- ``Name`` — the MCP tool name (an atom)
- ``Functor`` — the predicate functor
- ``Arity`` — the predicate arity

Tool descriptions and parameter schemas are automatically derived from
the ``info/2`` and ``mode/2`` directives. This must be accurate to allow
correct derivation of inout and output arguments and types.

The supported predicates types and their corresponding JSON types are:

============ ===========
Logtalk type JSON type
============ ===========
``integer``  ``integer``
``float``    ``number``
``number``   ``number``
``atom``     ``string``
``boolean``  ``boolean``
``list``     ``array``
``list(_)``  ``array``
``compound`` ``object``
``nonvar``   ``string``
``term``     ``string``
``chars``    ``string``
``codes``    ``string``
(other)      ``string``
============ ===========

.. _auto-dispatch-vs-custom-tool_call3:

Auto-dispatch vs. custom ``tool_call/3``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, when an MCP client calls a tool, the server auto-dispatches:
it calls the corresponding predicate as a message to the application
object, collects output-mode arguments (``-`` and ``--``), and returns
them as text content.

For custom result formatting, implement ``tool_call/3``:

.. code:: logtalk

   tool_call(factorial, Arguments, Result) :-
       member('N'-N, Arguments),
       factorial(N, F),
       number_codes(F, Codes),
       atom_codes(FAtom, Codes),
       atom_concat('The factorial is: ', FAtom, Text),
       Result = text(Text).

The ``Result`` term can be:

- ``text(Atom)`` — a text result
- ``error(Atom)`` — a tool-level error (sets ``isError: true``)
- ``results(List)`` — a list of content items (``text(Atom)`` or
  ``error(Atom)``)

Starting the server for debugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Start the MCP server from a Logtalk top-level or script:

.. code:: logtalk

   | ?- mcp_server::start('my-server', my_tools).

With options:

.. code:: logtalk

   | ?- mcp_server::start('my-server', my_tools, [version('2.0.0')]).

There should either be no output or only a Prolog backend term input
prompt. Other than that, any spurious output will break the connection
between a MCP client and the MCP server.

MCP client configuration
~~~~~~~~~~~~~~~~~~~~~~~~

To use the server with an MCP client (e.g., VSCode or Claude Desktop),
configure it as a stdio server. Example ``claude_desktop_config.json``:

.. code:: json

   {
       "mcpServers": {
           "my-server": {
               "command": "swilgt",
               "args": [
                   "-g", "logtalk_load(my_mcp_server(loader))",
                   "-t", "halt"
               ],
               "env": {
                   "LOGTALKHOME": "/usr/local/share/logtalk",
                   "LOGTALKUSER": "/Users/jdoe/logtalk"
               }
           }
       }
   }

The ``env`` definition of the ``LOGTALKHOME`` and ``LOGTALKUSER``
environment variables may or may not be required (it's usually necessary
on macOS). When required, replace the values above with the actual
values on your Logtalk setup.

The actual arguments to the integration script (``swilgt`` in the
example above) depend on the Prolog backend. For example, XVM requires
instead:

.. code:: json

   {
       "mcpServers": {
           "my-server": {
               "command": "xvmlgt",
               "args": [
                   "-g", "logtalk_load(my_mcp_server(loader)), halt.",
               ],
               "env": {
                   "LOGTALKHOME": "/usr/local/share/logtalk",
                   "LOGTALKUSER": "/Users/jdoe/logtalk"
               }
           }
       }
   }

Elicitation (interactive tools)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tools that need to ask the user questions during execution can use MCP
elicitation **if** the MCP client supports it (tested and working with
VSCode Copilot). The application declares the ``elicitation`` capability
and implements ``tool_call/4`` instead of ``tool_call/3``:

.. code:: logtalk

   :- object(interactive_tools,
       implements(mcp_tool_protocol)).

       capabilities([elicitation]).

       tools([
           tool(ask_name, ask_name, 0)
       ]).

       :- public(ask_name/0).
       :- info(ask_name/0, [
           comment is 'Asks the user for their name and greets them.'
       ]).

       tool_call(ask_name, _Arguments, Elicit, Result) :-
           Schema = {
               type-object,
               properties-{name-{type-string}},
               required-[name]
           },
           call(Elicit, 'What is your name?', Schema, Answer),
           (   Answer = accept(Content),
               has_pair(Content, name, Name) ->
               atom_concat('Hello, ', Name, Greeting),
               atom_concat(Greeting, '!', Text),
               Result = text(Text)
           ;   Result = text('No name provided.')
           ).

       has_pair({Pairs}, Key, Value) :-
           curly_member(Key-Value, Pairs).

       curly_member(Pair, (Pair, _)) :- !.
       curly_member(Pair, (_, Rest)) :-
           !, curly_member(Pair, Rest).
       curly_member(Pair, Pair).

   :- end_object.

The ``Elicit`` closure is called as
``call(Elicit, Message, Schema, Answer)`` where:

- ``Message`` — an atom with the prompt text
- ``Schema`` — a curly-term JSON Schema for the requested input
- ``Answer`` — unified with ``accept(Content)``, ``decline``, or
  ``cancel``

When ``accept(Content)`` is returned, ``Content`` is a curly-term with
the user's response matching the requested schema.

See the ``examples/birds_mcp/`` example for a complete demonstration of
elicitation with a bird identification expert system.

Error handling
--------------

- Predicate failures result in a tool-level error with
  ``isError: true``.
- Predicate exceptions result in a tool-level error with the exception
  term serialized as the error text.

Protocol
--------

The ``mcp_tool_protocol`` protocol defines the following predicates:

- ``capabilities/1`` — returns the list of additional capabilities
  needed by the application (e.g. ``[elicitation]``); optional, defaults
  to ``[]``
- ``tools/1`` — returns the list of tool descriptors
- ``tool_call/3`` — handles a tool call (optional; auto-dispatch is used
  when not defined)
- ``tool_call/4`` — handles a tool call with an elicitation closure
  (optional; requires ``capabilities([elicitation])``)

Supported MCP methods
---------------------

====================== ============ ==============================
Method                 Type         Description
====================== ============ ==============================
``initialize``         Request      MCP handshake
``initialized``        Notification Client acknowledgment
``ping``               Request      Server liveness check
``tools/list``         Request      Lists available tools
``tools/call``         Request      Calls a tool
``elicitation/create`` Request (=>) Asks the client for user input
====================== ============ ==============================
