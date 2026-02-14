.. _library_json_rpc:

``json_rpc``
============

JSON-RPC 2.0 protocol encoding and decoding library. Provides predicates
for constructing, parsing, classifying, and inspecting JSON-RPC 2.0
messages (requests, notifications, responses, and error responses). Also
provides stream-based message I/O for implementing JSON-RPC clients and
servers. Uses the ``json`` library for JSON parsing and generation.

API documentation
-----------------

Open the
`../../apis/library_index.html#json_rpc <../../apis/library_index.html#json_rpc>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(json_rpc(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(json_rpc(tester)).

Usage
-----

.. _constructing-json-rpc-20-messages:

Constructing JSON-RPC 2.0 messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Construct requests, notifications, responses, and error responses:

::

   | ?- json_rpc::request(subtract, [42,23], 1, Request).
   Request = {jsonrpc-'2.0', method-subtract, params-[42,23], id-1}.

   | ?- json_rpc::notification(update, [1,2,3], Notification).
   Notification = {jsonrpc-'2.0', method-update, params-[1,2,3]}.

   | ?- json_rpc::response(19, 1, Response).
   Response = {jsonrpc-'2.0', result-19, id-1}.

   | ?- json_rpc::error_response(-32601, 'Method not found', 1, ErrorResponse).

Standard error constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- json_rpc::parse_error(Error).
   Error = ...

   | ?- json_rpc::invalid_request(Error).
   Error = ...

   | ?- json_rpc::method_not_found(1, Error).
   Error = ...

   | ?- json_rpc::invalid_params(1, Error).
   Error = ...

   | ?- json_rpc::internal_error(1, Error).
   Error = ...

Encoding and decoding
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- json_rpc::request(subtract, [42,23], 1, Request),
      json_rpc::encode(Request, JSON).
   JSON = '{"jsonrpc":"2.0","method":"subtract","params":[42,23],"id":1}'.

   | ?- json_rpc::decode('{"jsonrpc":"2.0","result":19,"id":1}', Term).

Message classification
~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- json_rpc::is_request(Message).
   ...

   | ?- json_rpc::is_notification(Message).
   ...

   | ?- json_rpc::is_response(Message).
   ...

   | ?- json_rpc::is_error_response(Message).
   ...

   | ?- json_rpc::is_batch(Messages).
   ...

Field extraction
~~~~~~~~~~~~~~~~

::

   | ?- json_rpc::id(Message, Id).
   ...

   | ?- json_rpc::method(Message, Method).
   ...

   | ?- json_rpc::params(Message, Params).
   ...

   | ?- json_rpc::result(Message, Result).
   ...

   | ?- json_rpc::error_code(Message, Code).
   ...

   | ?- json_rpc::error_message(Message, ErrorMessage).
   ...

   | ?- json_rpc::error_data(Message, Data).
   ...

Stream I/O (client and server API)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write and read newline-delimited JSON-RPC messages over streams:

::

   | ?- json_rpc::write_message(Output, Message).
   ...

   | ?- json_rpc::read_message(Input, Message).
   ...

These predicates can be used with any stream, including socket streams
from the ``sockets`` library, to implement JSON-RPC clients and servers.

API summary
-----------

- ``request(+Method, +Params, +Id, --Request)`` - Construct a request
- ``request(+Method, +Id, --Request)`` - Construct a request with no
  parameters
- ``notification(+Method, +Params, --Notification)`` - Construct a
  notification
- ``notification(+Method, --Notification)`` - Construct a notification
  with no parameters
- ``response(+Result, +Id, --Response)`` - Construct a successful
  response
- ``error_response(+Code, +Message, +Id, --ErrorResponse)`` - Construct
  an error response
- ``error_response(+Code, +Message, +Data, +Id, --ErrorResponse)`` -
  Construct an error response with data
- ``parse_error(--ErrorResponse)`` - Construct a parse error (-32700)
- ``invalid_request(--ErrorResponse)`` - Construct an invalid request
  error (-32600)
- ``method_not_found(+Id, --ErrorResponse)`` - Construct a method not
  found error (-32601)
- ``invalid_params(+Id, --ErrorResponse)`` - Construct an invalid params
  error (-32602)
- ``internal_error(+Id, --ErrorResponse)`` - Construct an internal error
  (-32603)
- ``encode(+Term, --JSON)`` - Encode a JSON-RPC term to a JSON atom
- ``decode(+JSON, --Term)`` - Decode a JSON atom to a JSON-RPC term
- ``is_request(+Term)`` - Test if a term is a request
- ``is_notification(+Term)`` - Test if a term is a notification
- ``is_response(+Term)`` - Test if a term is a successful response
- ``is_error_response(+Term)`` - Test if a term is an error response
- ``is_batch(+Term)`` - Test if a term is a batch (non-empty list)
- ``id(+Message, --Id)`` - Extract the id field
- ``method(+Message, --Method)`` - Extract the method field
- ``params(+Message, --Params)`` - Extract the params field
- ``result(+Message, --Result)`` - Extract the result field
- ``error(+Message, --Error)`` - Extract the error object
- ``error_code(+Message, --Code)`` - Extract the error code
- ``error_message(+Message, --ErrorMessage)`` - Extract the error
  message
- ``error_data(+Message, --Data)`` - Extract the error data
- ``write_message(+Output, +Message)`` - Write a JSON-RPC message to a
  stream
- ``read_message(+Input, --Message)`` - Read a JSON-RPC message from a
  stream
