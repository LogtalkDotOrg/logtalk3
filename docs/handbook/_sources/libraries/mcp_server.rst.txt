.. _library_mcp_server:

``mcp_server``
==============

MCP (Model Context Protocol) server library for Logtalk applications.
Makes any Logtalk application available as a local MCP server using
stdio transport or Streamable HTTP transport.

Supports two specs, 2025-06-18 and 2026-07-28, and two transports, stdio
and streamable HTTP (with optional SSE for progress and long-lived
subscriptions).

- **2025-06-18** (default, stdio) - tools, prompts, resources,
  synchronous elicitation, structured output, resource links, version
  negotiation. Spec: ``mcp_server_2025_06_18_spec``.
- **2026-07-28** (stdio) - discovery, tools/prompts/resources,
  multi-round tool results (MRTR), caching, progress, subscriptions,
  cancellation. Adapter: ``mcp_server_2026_07_28_spec``.
- **2026-07-28** (Streamable HTTP) - same protocol semantics as the 2026
  stdio adapter, over HTTP POST with optional SSE for progress and
  long-lived subscriptions. Transport:
  ``mcp_server_streamable_http_transport``. Requires a multi-threaded
  backend.

The stdio adapters implement simple synchronous handling of server
requests. As a consequence, client ``notifications/cancelled`` can only
be used to drop matching subscription entries (i.e., listen cancel) and
cannot cancel in-flight work. The Streamable HTTP adapter supports
request-scoped SSE progress and subscription fan-out via ``notify/1``.

Specification references:

- https://modelcontextprotocol.io/specification/2025-06-18
- https://modelcontextprotocol.io/specification/2026-07-28

The library uses the ``json_rpc`` library for JSON-RPC 2.0 message
handling.

API documentation
-----------------

Open the
`../../apis/library_index.html#mcp-server <../../apis/library_index.html#mcp-server>`__
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

This runs the 2025-06-18, 2026-07-28 (stdio), and Streamable HTTP test
sets.

Architecture
------------

This library is designed to support adding new MCP specs and transports
by implementing ``mcp_server_adapter_protocol``. Common server code is
provided using the ``mcp_server_application`` category. A facade object,
``mcp_server``, allows selecting specific spec and transport using the
``spec/1`` and ``transport/1`` options. The legacy
``protocol_adapter/1`` option is still supported for backwards
compatibility.

The Streamable HTTP adapter additionally depends on the ``http_server``
library (and optionally ``http_sse`` helpers) for listening and framing.

There's also a set of protocols for the different MCP facets:

- ``mcp_tool_protocol``
- ``mcp_prompt_protocol``
- ``mcp_resource_protocol``
- ``mcp_multiround_protocol`` (2026 MRTR)
- ``mcp_cache_protocol`` (2026 cache policy)

An application object is only required to implement protocols for the
features it provides.

stdio versus Streamable HTTP transports
---------------------------------------

Both transports implement ``mcp_server_adapter_protocol``. What differs
is how JSON-RPC is carried and a few transport-only features.

+--------------------+--------------------------------+------------------------------------------+
|                    | stdio transport                | Streamable HTTP transport                |
+====================+================================+==========================================+
| Objects            | ``mcp_server_stdio_transport`` | ``mcp_server_streamable_http_transport`` |
+--------------------+--------------------------------+------------------------------------------+
| Transport          | Process stdin/stdout           | HTTP ``POST`` to a path (default         |
|                    | (newline-delimited JSON-RPC)   | ``/mcp``)                                |
+--------------------+--------------------------------+------------------------------------------+
| Spec versions      | 2025-06-18 or 2026-07-28       | 2025-06-18 (\*) or 2026-07-28            |
+--------------------+--------------------------------+------------------------------------------+
| Client model       | Client spawns the server as a  | Client talks to a listening URL          |
|                    | subprocess                     |                                          |
+--------------------+--------------------------------+------------------------------------------+
| I/O in ``start/4`` | Reads and writes the given     | Opens an ``http_server`` listener;       |
|                    | streams                        | stream arguments are unused              |
+--------------------+--------------------------------+------------------------------------------+
| Progress           | Stdio notifications when       | Optional SSE (``text/event-stream``)     |
|                    | applicable                     | when a ``progressToken`` is present      |
+--------------------+--------------------------------+------------------------------------------+
| Subscriptions      | Stdio listen loop              | Long-lived SSE plus ``notify/1`` fan-out |
+--------------------+--------------------------------+------------------------------------------+
| Extra options      | Spec options                   | Plus ``http_*`` options                  |
|                    | (``instructions``,             |                                          |
|                    | ``cache_*``, ...)              |                                          |
+--------------------+--------------------------------+------------------------------------------+

(\*) But no synchronous elicitation over plain POST.

Application objects do **not** change between transports. Only the
``spec/1`` and ``transport/1`` options selects the path:

::

   % stdio, 2025-06-18
   spec('2025-06-18'), transport(stdio)

   % stdio, 2026-07-28
   spec('2026-07-28'), transport(stdio)

   % Streamable HTTP, 2025-06-18
   spec('2025-06-18'), transport(streamable_http)

   % Streamable HTTP, 2026-07-28
   spec('2026-07-28'), transport(streamable_http)

Use **stdio** with desktop MCP clients that launch a command and speak
MCP on pipes. Use **Streamable HTTP** for remote or multi-client access,
reverse proxies, or clients that ``POST`` JSON-RPC (with optional SSE
for progress and subscriptions).

Always start servers through the ``mcp_server`` facade. For unit tests
or an external HTTP stack, the Streamable HTTP adapter also exposes
``prepare/2``, ``handle_mcp_request/4``, and ``cleanup/0`` predicates
without opening a listener.

Starting a MCP server
---------------------

Starting a MCP server requires at least a server name and the
application server object and optionally a list of options to customize
the server. Always use the ``mcp_server`` facade object to start a
server (the adapters are not meant to be used directly). Some examples,
assuming a ``my_tools`` application object:

.. _2025-06-18-spec-and-stdio-transport-default:

2025-06-18 spec and stdio transport (default)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- mcp_server::start('my-server', my_tools).

With options:

::

   | ?- mcp_server::start('my-server', my_tools, [
           server_version('2.0.0'),
           server_title('My Server')
       ]).

.. _2026-07-28-spec-and-stdio-transport:

2026-07-28 spec and stdio transport
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- mcp_server::start('my-server', my_tools, [
           spec('2026-07-28'),
           server_version('2.0.0'),
           server_title('My Server'),
           instructions('Optional server instructions for clients.'),
           cache_ttl(0),
           cache_scope(private)
       ]).

For stdio transports there should either be no standard output or only a
Prolog backend term input prompt. Spurious standard output will break
the connection between an MCP client and the MCP server.

.. _2026-07-28-spec-and-streamable-http:

2026-07-28 spec and Streamable HTTP
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- mcp_server::start('my-server', my_tools, [
           spec('2026-07-28'),
           transport(streamable_http),
           server_version('2.0.0'),
           server_title('My Server'),
           instructions('Optional server instructions for clients.'),
           http_port(8080),
           http_bind('127.0.0.1'),
           http_path('/mcp'),
           http_origin_check(true)
       ]).

The server listens for ``POST`` requests at the configured bind address,
port, and path (default ``http://127.0.0.1:8080/mcp``). Clients must
send ``MCP-Protocol-Version: 2026-07-28`` and a JSON-RPC body using the
2026 ``_meta`` conventions. When a ``progressToken`` is present, the
response may use ``text/event-stream`` (SSE) for progress events and the
final result.

For unit tests and embedded HTTP stacks, call
``mcp_server_streamable_http_transport::prepare/2`` then
``handle_mcp_request/4`` without starting the listener, and finish with
``cleanup/0``.

Common options
--------------

+-----------------------------+--------------------------+----------------------+
| Option                      | Default                  | Description          |
+=============================+==========================+======================+
| ``spec(Spec)``              | ``'2025-06-18'``         | Spec selection       |
+-----------------------------+--------------------------+----------------------+
| ``transport(Transport)``    | ``stdio``                | Transport selection  |
+-----------------------------+--------------------------+----------------------+
| ``server_version(Version)`` | ``'1.0.0'``              | Server version       |
|                             |                          | string               |
+-----------------------------+--------------------------+----------------------+
| ``server_title(Title)``     | ``'logtalk-mcp-server'`` | Display title        |
+-----------------------------+--------------------------+----------------------+

.. _2026-07-28-spec-specific-options:

2026-07-28 spec specific options
--------------------------------

+-----------------------------+-------------+---------------------------+
| Option                      | Default     | Description               |
+=============================+=============+===========================+
| ``instructions(Text)``      | ``''``      | Optional instructions     |
|                             |             | (2026 discover)           |
+-----------------------------+-------------+---------------------------+
| ``cache_ttl(Milliseconds)`` | ``0``       | Default TTL in            |
|                             |             | milliseconds (2026)       |
+-----------------------------+-------------+---------------------------+
| ``cache_scope(Scope)``      | ``private`` | ``public`` or ``private`` |
|                             |             | (2026)                    |
+-----------------------------+-------------+---------------------------+

Streamable HTTP adapter options
-------------------------------

+---------------------------------+-----------------+--------------------------+
| Option                          | Default         | Description              |
+=================================+=================+==========================+
| ``http_port(Port)``             | ``8080``        | TCP port to listen on    |
+---------------------------------+-----------------+--------------------------+
| ``http_bind(Address)``          | ``'127.0.0.1'`` | Bind address             |
+---------------------------------+-----------------+--------------------------+
| ``http_path(Path)``             | ``'/mcp'``      | HTTP path for MCP POST   |
|                                 |                 | requests                 |
+---------------------------------+-----------------+--------------------------+
| ``http_origin_check(Flag)``     | ``true``        | Reject disallowed        |
|                                 |                 | ``Origin`` headers when  |
|                                 |                 | ``true``                 |
+---------------------------------+-----------------+--------------------------+
| ``http_sse_keepalive(Seconds)`` | ``15``          | Keep-alive interval for  |
|                                 |                 | ``subscriptions/listen`` |
|                                 |                 | response streams         |
+---------------------------------+-----------------+--------------------------+

These options are validated by the ``mcp_server`` facade and applied
only when ``protocol_adapter(mcp_server_streamable_http_transport)`` is
selected.

Implementing the tool protocol
------------------------------

To expose a Logtalk object as an MCP tool provider, implement the
``mcp_tool_protocol`` protocol. For example:

::

   :- object(my_tools,
       implements(mcp_tool_protocol)).

       :- public(factorial/2).
       :- mode(factorial(+integer, -integer), one).
       :- info(factorial/2, [
           comment is 'Computes the factorial of a non-negative integer.',
           argnames is ['N', 'F']
       ]).

       :- uses(natural, [
           factorial/2
       ]).

       tools([
           tool(factorial, factorial, 2)
       ]).

   :- end_object.

The ``tools/1`` predicate returns a list of
``tool(Name, Functor, Arity)`` descriptors. Tool descriptions, input
schemas, and output schemas are derived from the ``info/2`` and
``mode/2`` directives. Input-mode arguments (``+``, ``++``, and ``@``)
define ``inputSchema``; output-mode arguments (``-`` and ``--``) define
``outputSchema``. A ``title`` key in the predicate's ``info/2``
directive provides a human-friendly display name. If omitted, the
predicate functor is used.

Supported Logtalk types and their JSON Schema counterparts:

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

Auto-dispatch
~~~~~~~~~~~~~

By default the server auto-dispatches a tool call: it binds the
input-mode arguments (``+``, ``++``, and ``@``), calls the corresponding
predicate, collects the output-mode arguments (``-`` and ``--``), and
returns them as ``structuredContent``. For backwards compatibility, it
also returns the existing human-readable rendering as a ``text/1``
content item. The tool descriptor's ``inputSchema`` and ``outputSchema``
are inferred from the predicate documentation. If the predicate fails,
the server returns an MCP tool error stating ``Tool predicate failed``;
if it throws an exception, the exception is returned as the tool error.

Overriding input/output schemas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tools can override an inferred input or output schema by defining
``input_schema/2`` or ``output_schema/2`` in their application object:

::

   input_schema(factorial, {
       type-object,
       properties-{'N'-{type-integer}}, required-['N']
   }).

   output_schema(factorial, {
       type-object,
       properties-{'F'-{type-integer}}, required-['F']
   }).

The tool descriptor always includes ``inputSchema`` and ``outputSchema``
fields.

Defining ``output_schema/2`` is usually required when a tool predicate
has no arguments (which results in an inferred empty output schema) to
ensure wide compatibility with clients. For example, assuming a text
output that we want to ensure that is received by all clients:

::

   output_schema(tool_predicate, {
       type-object,
       properties-{message-{type-string}},
       required-[message]
   }).

Combine this with a ``tool_call/3`` predicate definition (see below)
that binds the result argument to either
``structured([text(Text)], {message-Text})`` or
``structured({message-Text})``.

Custom result formatting
~~~~~~~~~~~~~~~~~~~~~~~~

For custom result formatting, including a ``structuredContent`` result
matching the tool's output schema, implement ``tool_call/3``. As an
output schema is always advertised, a successful custom result must use
``structured/1`` or ``structured/2`` and conform to that schema. The
``Result`` term can be:

- ``structured(StructuredContent)`` - structured output with auto text
- ``structured(Items, StructuredContent)`` - structured output with
  explicit content items

The content items can be:

- ``text(Atom)`` - a text result
- ``error(Atom)`` - a tool-level error (``isError: true``)
- ``results(List)`` - content items (``text/1``, ``error/1``,
  ``resource_link/2``, ``resource_link/4``)

The ``StructuredContent`` argument must be a curly-term matching the
schema.

For example:

::

   tool_call(factorial, Arguments, Result) :-
       member('N'-N, Arguments),
       factorial(N, F),
       number_codes(F, Codes),
       atom_codes(FAtom, Codes),
       atom_concat('The factorial is: ', FAtom, Text),
       Result = structured([text(Text)], {'F'-F}).

The ``tool_call/3`` predicate is specially useful for tool predicates
with no arguments. In this case, custom text can be used to explain
success or failure of the tool predicate by returning a
``structured([text(Explanation)], {})`` result. Note that ``{}`` is the
output schema when a predicate have no output arguments.

Elicitation (2025-06-18 spec)
-----------------------------

Under the **2025-06-18** adapter, tools that need to ask the user
question during execution can use MCP elicitation **if** the MCP client
supports it (tested and working with VSCode Copilot). The application
declares that it requires the client ``elicitation`` capability and
implements ``tool_call/4`` instead of ``tool_call/3``. The extra
argument is an elicitation closure. For example:

::

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

- ``Message`` - an atom with the prompt text
- ``Schema`` - a curly-term JSON Schema for the requested input
- ``Answer`` - unified with ``accept(Content)``, ``decline``, or
  ``cancel``

When ``accept(Content)`` is returned, ``Content`` is a curly-term with
the user's response matching the requested schema.

See the ``examples/birds_mcp/`` example for a complete demonstration of
elicitation with a bird identification expert system.

Note that the 2026-07-28 adapter **never** invokes ``tool_call/4``.
Multi-round interaction for the 2026-07-28 spec uses
``mcp_multiround_protocol`` instead.

Multi-round tool results - MRTR (2026-07-28 spec)
-------------------------------------------------

Implement ``mcp_multiround_protocol`` and define round hooks:

- ``tool_call_round/4``
- ``prompt_get_round/4``
- ``resource_read_round/4``

Each receives a
``request_context(ClientCapabilities, InputResponses, RequestState, Progress)``
term and returns either:

- ``complete(Result)`` - using the existing canonical result vocabulary
- ``input_required(InputRequests, RequestState)`` - request more input

``InputRequests`` is a list of uniquely keyed
``input_request(Key, Request)`` terms. Allowed request forms:

- ``form_elicitation(Message, Schema)``
- ``url_elicitation(Message, URL)``
- ``sampling(Messages, ModelPreferences, SystemPrompt, IncludeContext)``
- ``roots``

``RequestState`` is application-owned opaque data (or ``none``).
Applications that use it for authorization or business decisions must
integrity-protect and validate it themselves.

Existing applications that do not implement the round hooks continue
through ``tool_call/3``, ``prompt_get/3``, ``resource_read/3``, or
auto-dispatch; the 2026 adapter wraps those outcomes as ``complete``.

Example:

::

   :- object(interactive,
       implements([mcp_tool_protocol, mcp_multiround_protocol])).

       tools([tool(ask_name, ask_name, 0)]).

       tool_call_round(ask_name, _Args, Context, RoundResult) :-
           Context = request_context(_Caps, Responses, State, _Progress),
           (   State == none ->
               RoundResult = input_required(
                   [input_request(name_key, form_elicitation('Your name?', {type-object, properties-{name-{type-string}}, required-[name]}))],
                   waiting
               )
           ;   member(input_response(name_key, accept(Content)), Responses) ->
               % extract name, return complete(text(...))
               RoundResult = complete(text('Hello!'))
           ;   RoundResult = complete(text('Cancelled.'))
           ).

   :- end_object.

Caching (2026-07-28 spec)
-------------------------

Optional ``mcp_cache_protocol`` with ``cache_policy/4``:

::

   cache_policy(tools_list, _, 1000, private).
   cache_policy(resources_read, 'logtalk://app/data', 5000, public).

Cache fields (``ttlMs``, ``cacheScope``) are attached only to complete
results of ``server/discover``, list operations, and ``resources/read``.
They are never attached to ``input_required`` results or MRTR retries.

Defaults: ``cache_ttl(0)``, ``cache_scope(private)``.

Progress (2026-07-28 spec)
--------------------------

When the client supplies a ``progressToken`` in request ``_meta``, the
``Progress`` closure in ``request_context`` can emit
``notifications/progress``. Progress is suppressed after cancellation or
completion.

Subscriptions and notifications (2026-07-28 spec)
-------------------------------------------------

Clients call ``subscriptions/listen`` with filters. The server
acknowledges first (``notifications/subscriptions/acknowledged``), then
delivers matching events.

Applications publish events via:

::

   mcp_server::notify(tools_list_changed).
   mcp_server::notify(prompts_list_changed).
   mcp_server::notify(resources_list_changed).
   mcp_server::notify(resource_updated('logtalk://app/data')).

The facade delegates to the active adapter. The 2025-06-18 adapter
ignores these events. The 2026-07-28 stdio adapter and the Streamable
HTTP adapter route them through active subscriptions. On HTTP,
long-lived SSE connections from ``subscriptions/listen`` receive
matching events; ``notify/1`` isolates per-subscriber failures so a dead
stream does not abort delivery to others.

Prompts
-------

MCP prompts are templates for structured LLM interactions. They allow an
application to expose reusable prompt templates that MCP clients can
discover and use. To add prompts, implement ``mcp_prompt_protocol`` in
addition to ``mcp_tool_protocol``, and declare ``prompts`` in
capabilities:

::

   :- object(my_prompts,
       implements([mcp_tool_protocol, mcp_prompt_protocol])).

       :- uses(list, [member/2]).

       capabilities([prompts]).

       tools([]).

       prompts([
           prompt(code_review, 'Reviews code for potential issues', [
               argument(code, 'The code to review', true),
               argument(language, 'The programming language', false)
           ]),
           prompt(summarize, 'Summarizes a given text', [
               argument(text, 'The text to summarize', true)
           ])
       ]).

       prompt_get(code_review, Arguments, Result) :-
           (   member(code-Code, Arguments) ->
               atom_concat('Please review the following code for potential issues:\n\n', Code, Text)
           ;   Text = 'Please provide code to review.'
           ),
           Result = messages([message(user, text(Text))]).

       prompt_get(summarize, Arguments, Result) :-
           (   member(text-Text, Arguments) ->
               atom_concat('Please summarize the following text:\n\n', Text, PromptText)
           ;   PromptText = 'Please provide text to summarize.'
           ),
           Result = messages([message(user, text(PromptText))]).

   :- end_object.

The ``prompts/1`` predicate returns a list of prompt descriptors:

- ``prompt(Name, Description, Arguments)`` - without title
- ``prompt(Name, Title, Description, Arguments)`` - with title

Where:

- ``Name`` - the MCP prompt name (an atom)
- ``Title`` - a human-friendly display name (an atom, optional)
- ``Description`` - a human-readable description (an atom)
- ``Arguments`` - a list of
  ``argument(ArgName, ArgDescription, Required)`` terms where
  ``Required`` is ``true`` or ``false``

The ``prompt_get/3`` predicate handles prompt get requests. Its result
term can be:

- ``messages(MessageList)`` - a list of prompt messages
- ``messages(Description, MessageList)`` - a list of messages with a
  description

Each message in the list is a ``message(Role, Content)`` term where:

- ``Role`` - ``user`` or ``assistant``
- ``Content`` - ``text(Text)`` where ``Text`` is an atom

Multi-turn prompts can return multiple messages:

::

   prompt_get(debate, Arguments, Result) :-
       member(topic-Topic, Arguments),
       atom_concat('Let us debate: ', Topic, UserText),
       Result = messages([
           message(user, text(UserText)),
           message(assistant, text('I would be happy to debate that topic. What is your position?'))
       ]).

For 2026 multi-round prompts, implement ``prompt_get_round/4``.

Resources
---------

MCP resources expose data and content from the application that MCP
clients can access. To add resources, implement
``mcp_resource_protocol`` in addition to ``mcp_tool_protocol``, and
declare ``resources`` in capabilities:

::

   :- object(my_resources,
       implements([mcp_tool_protocol, mcp_resource_protocol])).

       capabilities([resources]).

       tools([]).

       resources([
           resource('logtalk://my-app/config', config, 'Application configuration', 'application/json'),
           resource('logtalk://my-app/readme', readme, 'Application readme', 'text/plain')
       ]).

       resource_read('logtalk://my-app/config', _Arguments, Result) :-
           Result = contents([
               text_content('logtalk://my-app/config', 'application/json', '{"name": "my-app", "version": "1.0"}')
           ]).

       resource_read('logtalk://my-app/readme', _Arguments, Result) :-
           Result = contents([
               text_content('logtalk://my-app/readme', 'text/plain', 'Welcome to my application.')
           ]).

   :- end_object.

The ``resources/1`` predicate returns a list of resource descriptors:

- ``resource(URI, Name, Description, MimeType)`` - without title
- ``resource(URI, Name, Title, Description, MimeType)`` - with title

Where:

- ``URI`` - the resource identifier (an atom, typically a URI like
  ``logtalk://my-app/data``)
- ``Name`` - a human-readable name (an atom)
- ``Title`` - a human-friendly display name (an atom, optional)
- ``Description`` - a human-readable description (an atom)
- ``MimeType`` - the MIME type of the resource content (an atom, e.g.
  ``'text/plain'``, ``'application/json'``)

The ``resource_read/3`` predicate handles resource read requests. Its
result term must be ``contents(ContentList)`` where each content item
is:

- ``text_content(URI, MimeType, Text)`` - for text resources
- ``blob_content(URI, MimeType, Base64Data)`` - for binary resources
  encoded as base64

A resource can return multiple content items. For example:

::

   resource_read('logtalk://my-app/logs', _Arguments, Result) :-
       Result = contents([
           text_content('logtalk://my-app/logs', 'text/plain', 'Log entry 1'),
           text_content('logtalk://my-app/logs', 'text/plain', 'Log entry 2')
       ]).

For 2026 multi-round reads, implement ``resource_read_round/4``.

MCP client configuration
------------------------

Example ``claude_desktop_config.json`` for the 2025-06-18 path:

::

   {
       "mcpServers": {
           "my-server": {
               "command": "swilgt",
               "args": [
                   "-q",
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

::

   {
       "mcpServers": {
           "my-server": {
               "command": "xvmlgt",
               "args": [
                   "-q",
                   "-g", "logtalk_load(my_mcp_server(loader)), halt.",
               ],
               "env": {
                   "LOGTALKHOME": "/usr/local/share/logtalk",
                   "LOGTALKUSER": "/Users/jdoe/logtalk"
               }
           }
       }
   }

For a 2026-07-28 stdio server, the application loader or start goal must
pass ``protocol_adapter(mcp_server_2026_07_28_adapter)``.

For Streamable HTTP, start the server with
``protocol_adapter(mcp_server_streamable_http_transport)`` and point the
MCP client at the listen URL (for example
``http://127.0.0.1:8080/mcp``). Each request should include:

- ``Content-Type: application/json``
- ``Accept: application/json, text/event-stream``
- ``MCP-Protocol-Version: 2026-07-28``

Error handling
--------------

.. _2025-06-18-spec:

2025-06-18 spec
~~~~~~~~~~~~~~~

- Predicate failures result in a tool-level error with
  ``isError: true``.
- Predicate exceptions result in a tool-level error with the exception
  term serialized as the error text.
- Prompt execution failures result in a JSON-RPC error response.
- Resource read failures result in a JSON-RPC error response.

.. _2026-07-28-spec:

2026-07-28 spec
~~~~~~~~~~~~~~~

+------------+---------------------------------------------------------+
| Code       | Meaning                                                 |
+============+=========================================================+
| ``-32602`` | Missing/malformed required metadata or invalid          |
|            | arguments                                               |
+------------+---------------------------------------------------------+
| ``-32022`` | Unsupported protocol version (``data.supported``,       |
|            | ``data.requested``)                                     |
+------------+---------------------------------------------------------+
| ``-32021`` | Missing required client capability                      |
|            | (``data.requiredCapabilities``)                         |
+------------+---------------------------------------------------------+
| ``-32601`` | Unknown or unadvertised method                          |
+------------+---------------------------------------------------------+
| ``-32603`` | Internal / execution failure                            |
+------------+---------------------------------------------------------+

Protocols overview
------------------

Follows a list of the main predicates declared in the protocols meant to
be implemented by an application. See the API documentation for full
details.

``mcp_tool_protocol``
~~~~~~~~~~~~~~~~~~~~~

- ``capabilities/1`` - returns the list of additional features needed by
  the application (e.g. ``[elicitation]``, ``[prompts]``,
  ``[resources]``, or ``[prompts, resources, elicitation]``);
  ``prompts`` and ``resources`` are server capabilities while
  ``elicitation`` is a required client capability; optional, defaults to
  ``[]``
- ``tools/1`` - returns the list of tool descriptors
- ``tool_call/3`` - handles a tool call (optional; auto-dispatch is used
  when not defined)
- ``tool_call/4`` - handles a tool call with an elicitation closure
  (optional; requires ``capabilities([elicitation])`` or
  ``capabilities([..., elicitation])``; **2025-06-18 only**)
- ``input_schema/2`` - overrides the inferred JSON Schema for tool input
  (optional)
- ``output_schema/2`` - overrides the inferred JSON Schema for
  structured tool output (optional)

``mcp_prompt_protocol``
~~~~~~~~~~~~~~~~~~~~~~~

- ``prompts/1`` - returns the list of prompt descriptors
- ``prompt_get/3`` - handles a prompt get request

``mcp_resource_protocol``
~~~~~~~~~~~~~~~~~~~~~~~~~

- ``resources/1`` - returns the list of resource descriptors
- ``resource_read/3`` - handles a resource read request

``mcp_multiround_protocol`` (2026-07-28)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``tool_call_round/4`` - handles one round of a multi-round tool call
- ``prompt_get_round/4`` - handles one round of a multi-round prompt get
- ``resource_read_round/4`` - handles one round of a multi-round
  resource read

``mcp_cache_protocol`` (2026-07-28)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- ``cache_policy/4`` - optional per-operation TTL and scope

Supported MCP methods per spec
------------------------------

.. _2025-06-18-spec-1:

2025-06-18 spec
~~~~~~~~~~~~~~~

+-------------------------------+----------------------+----------------------+
| Method                        | Type                 | Description          |
+===============================+======================+======================+
| ``initialize``                | Request              | Handshake and        |
|                               |                      | version negotiation  |
+-------------------------------+----------------------+----------------------+
| ``notifications/initialized`` | Notification         | Client               |
|                               |                      | acknowledgment       |
+-------------------------------+----------------------+----------------------+
| ``ping``                      | Request              | Liveness check       |
+-------------------------------+----------------------+----------------------+
| ``tools/list``                | Request              | List tools           |
+-------------------------------+----------------------+----------------------+
| ``tools/call``                | Request              | Call a tool          |
+-------------------------------+----------------------+----------------------+
| ``prompts/list``              | Request              | List prompts         |
+-------------------------------+----------------------+----------------------+
| ``prompts/get``               | Request              | Get a prompt         |
+-------------------------------+----------------------+----------------------+
| ``resources/list``            | Request              | List resources       |
+-------------------------------+----------------------+----------------------+
| ``resources/read``            | Request              | Read a resource      |
+-------------------------------+----------------------+----------------------+
| ``elicitation/create``        | Request (server ->   | Ask the user for     |
|                               | client)              | input                |
+-------------------------------+----------------------+----------------------+

.. _2026-07-28-spec-1:

2026-07-28 spec
~~~~~~~~~~~~~~~

+----------------------------------------------+----------------------+----------------------+
| Method                                       | Type                 | Description          |
+==============================================+======================+======================+
| ``server/discover``                          | Request              | Discovery (replaces  |
|                                              |                      | initialize)          |
+----------------------------------------------+----------------------+----------------------+
| ``ping``                                     | Request              | Liveness check       |
+----------------------------------------------+----------------------+----------------------+
| ``tools/list``                               | Request              | List tools           |
+----------------------------------------------+----------------------+----------------------+
| ``tools/call``                               | Request              | Call a tool          |
|                                              |                      | (supports MRTR)      |
+----------------------------------------------+----------------------+----------------------+
| ``prompts/list``                             | Request              | List prompts         |
+----------------------------------------------+----------------------+----------------------+
| ``prompts/get``                              | Request              | Get a prompt         |
|                                              |                      | (supports MRTR)      |
+----------------------------------------------+----------------------+----------------------+
| ``resources/list``                           | Request              | List resources       |
+----------------------------------------------+----------------------+----------------------+
| ``resources/read``                           | Request              | Read a resource      |
|                                              |                      | (supports MRTR)      |
+----------------------------------------------+----------------------+----------------------+
| ``subscriptions/listen``                     | Request              | Open a subscription  |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/cancelled``                  | Notification         | Cancel in-flight     |
|                                              |                      | request              |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/progress``                   | Notification (server | Progress update      |
|                                              | -> client)           |                      |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/subscriptions/acknowledged`` | Notification (server | Subscription ack     |
|                                              | -> client)           |                      |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/tools/list_changed``         | Notification (server | Tools changed        |
|                                              | -> client)           |                      |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/prompts/list_changed``       | Notification (server | Prompts changed      |
|                                              | -> client)           |                      |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/resources/list_changed``     | Notification (server | Resources changed    |
|                                              | -> client)           |                      |
+----------------------------------------------+----------------------+----------------------+
| ``notifications/resources/updated``          | Notification (server | Resource updated     |
|                                              | -> client)           |                      |
+----------------------------------------------+----------------------+----------------------+

The 2026-07-28 stdio adapter never writes JSON-RPC **requests** to
stdout (only responses and notifications). The Streamable HTTP adapter
likewise only returns responses and server-initiated notifications
(progress and subscription events), never client-bound requests over the
HTTP response channel.

MCP Apps (interactive UI)
-------------------------

MCP Apps (``io.modelcontextprotocol/ui``) lets tools declare an
interactive HTML UI that hosts render in a sandboxed iframe. The
**server** only serves tools and ``ui://`` resources; host <-> iframe
traffic is handled by the host.

Compatible with **2025-06-18** and **2026-07-28**, and with **stdio**
and **Streamable HTTP**. Spec:

- https://modelcontextprotocol.io/extensions/apps/overview

Declaring the extension
~~~~~~~~~~~~~~~~~~~~~~~

::

   capabilities([resources, ui]).

Advertises ``extensions["io.modelcontextprotocol/ui"]`` in
``initialize`` (2025) or ``server/discover`` (2026).

UI resources
~~~~~~~~~~~~

::

   resources([
     resource(
       'ui://my-app/dashboard',
       dashboard,
       'Interactive dashboard',
       'text/html;profile=mcp-app'
     )
   ]).

Optional CSP via ``resource_ui_meta/2`` (``mcp_ui_protocol``).

Linking tools to UI
~~~~~~~~~~~~~~~~~~~

::

   tool_ui(show_dashboard, [
     resource_uri('ui://my-app/dashboard'),
     visibility([model, app])
   ]).

``tools/list`` includes ``_meta.ui.resourceUri`` /
``_meta.ui.visibility``.

Out of scope
~~~~~~~~~~~~

Host <-> iframe JSON-RPC (``ui/initialize``, sandbox, ``postMessage``).

Limitations
-----------

Resource templates, completion, authorization, and optional
extensions/tasks are not currently implemented. Streamable HTTP
transport is implemented by ``mcp_server_streamable_http_transport``
(2026-07-28 protocol semantics over HTTP POST with optional SSE).
