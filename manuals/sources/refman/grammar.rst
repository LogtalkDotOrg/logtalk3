..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


.. _grammar_grammar:

Grammar
=======

The Logtalk grammar is here described using W3C-style Extended Backus-Naur
Form syntax. Non-terminal symbols not defined here can be found in the ISO
Prolog Core standard. Terminal symbols are represented between double-quotes.

.. _grammar_entities:

Entities
--------

.. code-block:: bnf

   entity ::=
      object
      | category
      | protocol

.. _grammar_object_definition:

Object definition
-----------------

.. code-block:: bnf

   object ::=
      begin_object_directive ( object_term )* end_object_directive

   begin_object_directive ::=
      ":- object(" object_identifier ( "," object_relations )? ")."

   end_object_directive ::=
      ":- end_object."

   object_relations ::=
      prototype_relations
      | non_prototype_relations

   prototype_relations ::=
      prototype_relation
      | prototype_relation "," prototype_relations

   prototype_relation ::=
      implements_protocols
      | imports_categories
      | extends_objects

   non_prototype_relations ::=
      non_prototype_relation
      | non_prototype_relation "," non_prototype_relations

   non_prototype_relation ::=
      implements_protocols
      | imports_categories
      | instantiates_classes
      | specializes_classes

.. _grammar_category_definition:

Category definition
-------------------

.. code-block:: bnf

   category ::=
      begin_category_directive  ( category_term )* end_category_directive

   begin_category_directive ::=
      ":- category(" category_identifier ( "," category_relations )? ")."

   end_category_directive ::=
      ":- end_category."

   category_relations ::=
      category_relation
      | category_relation "," category_relations

   category_relation ::=
      implements_protocols
      | extends_categories
      | complements_objects

.. _grammar_protocol_definition:

Protocol definition
-------------------

.. code-block:: bnf

   protocol ::=
      begin_protocol_directive  ( protocol_directive )* end_protocol_directive

   begin_protocol_directive ::=
      ":- protocol(" protocol_identifier ( "," extends_protocols)? ")."

   end_protocol_directive ::=
      ":- end_protocol."

.. _grammar_entity_relations:

Entity relations
----------------

.. code-block:: bnf

   extends_protocols ::=
      "extends(" extended_protocols ")"

   extends_objects ::=
      "extends(" extended_objects ")"

   extends_categories ::=
      "extends(" extended_categories ")"

   implements_protocols ::=
      "implements(" implemented_protocols ")"

   imports_categories ::=
      "imports(" imported_categories ")"

   instantiates_classes ::=
      "instantiates(" instantiated_objects ")"

   specializes_classes ::=
      "specializes(" specialized_objects ")"

   complements_objects ::=
      "complements(" complemented_objects ")"

.. _grammar_implemented_protocols:

Implemented protocols
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   implemented_protocols ::=
      implemented_protocol
      | implemented_protocol_sequence
      | implemented_protocol_list

   implemented_protocol ::=
      protocol_identifier
      | scope "::" protocol_identifier

   implemented_protocol_sequence ::=
      implemented_protocol
      | implemented_protocol "," implemented_protocol_sequence

   implemented_protocol_list ::=
      "[" implemented_protocol_sequence "]"

.. _grammar_extended_protocols:

Extended protocols
~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   extended_protocols ::=
      extended_protocol
      | extended_protocol_sequence
      | extended_protocol_list

   extended_protocol ::=
      protocol_identifier
      | scope "::" protocol_identifier

   extended_protocol_sequence ::=
      extended_protocol
      |extended_protocol "," extended_protocol_sequence

   extended_protocol_list ::=
      "[" extended_protocol_sequence "]"

.. _grammar_imported_categories:

Imported categories
~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   imported_categories ::=
      imported_category
      | imported_category_sequence
      | imported_category_list

   imported_category ::=
      category_identifier
      | scope "::" category_identifier

   imported_category_sequence ::=
      imported_category
      | imported_category "," imported_category_sequence

   imported_category_list ::=
      "[" imported_category_sequence "]"

.. _grammar_extended_objects:

Extended objects
~~~~~~~~~~~~~~~~

.. code-block:: bnf

   extended_objects ::=
      extended_object
      | extended_object_sequence
      | extended_object_list

   extended_object ::=
      object_identifier
      | scope "::" object_identifier

   extended_object_sequence ::=
      extended_object
      | extended_object "," extended_object_sequence

   extended_object_list ::=
      "[" extended_object_sequence "]"

.. _grammar_extended_categories:

Extended categories
~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   extended_categories ::=
      extended_category
      | extended_category_sequence
      | extended_category_list

   extended_category ::=
      category_identifier
      | scope "::" category_identifier

   extended_category_sequence ::=
      extended_category
      | extended_category "," extended_category_sequence

   extended_category_list ::=
      "[" extended_category_sequence "]"

.. _grammar_instantiated_objects:

Instantiated objects
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   instantiated_objects ::=
      instantiated_object
      | instantiated_object_sequence
      | instantiated_object_list

   instantiated_object ::=
      object_identifier
      | scope "::" object_identifier

   instantiated_object_sequence ::=
      instantiated_object
      | instantiated_object "," instantiated_object_sequence

   instantiated_object_list ::=
      "[" instantiated_object_sequence "]"

.. _grammar_specialized_objects:

Specialized objects
~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   specialized_objects ::=
      specialized_object
      | specialized_object_sequence
      | specialized_object_list

   specialized_object ::=
      object_identifier
      | scope "::" object_identifier

   specialized_object_sequence ::=
      specialized_object
      | specialized_object "," specialized_object_sequence

   specialized_object_list ::=
      "[" specialized_object_sequence "]"

.. _grammar_complemented_objects:

Complemented objects
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   complemented_objects ::=
      object_identifier
      | complemented_object_sequence
      | complemented_object_list

   complemented_object_sequence ::=
      object_identifier
      | object_identifier "," complemented_object_sequence

   complemented_object_list ::=
      "[" complemented_object_sequence "]"

.. _grammar_scope:

Entity and predicate scope
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   scope ::=
      "public"
      | "protected"
      | "private"

.. _grammar_entity_identifiers:

Entity identifiers
------------------

.. code-block:: bnf

   entity_identifier ::=
      object_identifier
      | protocol_identifier
      | category_identifier

.. _grammar_object_identifiers:

Object identifiers
~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   object_identifier ::=
      atom
      | compound

.. _grammar_category_identifiers:

Category identifiers
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   category_identifier ::=
      atom
      | compound

.. _grammar_protocol_identifiers:

Protocol identifiers
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   protocol_identifier ::=
      atom

.. _grammar_module_identifiers:

Module identifiers
~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   module_identifier ::=
      atom

.. _grammar_source_files:

Source files
------------

.. code-block:: bnf

   source_file ::=
      ( source_file_content )*

   source_file_content ::=
      source_file_directive
      | clause
      | grammar_rule
      | entity

.. _grammar_source_file_names:

Source file names
-----------------

.. code-block:: bnf

   source_file_name ::=
      atom
      | library_source_file_name

   library_source_file_name ::=
      library_name "(" atom ")"

   library_name ::=
      atom

.. _grammar_terms:

Terms
-----

.. _grammar_object_terms:

Object terms
~~~~~~~~~~~~

.. code-block:: bnf

   object_term ::=
      object_directive
      | clause
      | grammar_rule

.. _grammar_category_terms:

Category terms
~~~~~~~~~~~~~~

.. code-block:: bnf

   category_term ::=
      category_directive
      | clause
      | grammar_rule

.. _grammar_directives:

Directives
----------

.. _grammar_source_file_directives:

Source file directives
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   source_file_directive ::=
      ":- encoding(" atom ")."
      | ":- set_logtalk_flag(" atom "," nonvar ")."
      | ":- include(" source_file_name ")."
      | prolog_directive

.. _grammar_conditional_compilation_directives:

Conditional compilation directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   conditional_compilation_directive ::=
      ":- if(" callable ")."
      | ":- elif(" callable ")."
      | ":- else."
      | ":- endif."

.. _grammar_object_directives:

Object directives
~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   object_directive ::=
      ":- initialization(" callable ")."
      | ":- built_in."
      | ":- threaded."
      | ":- dynamic."
      | ":- info(" entity_info_list ")."
      | ":- set_logtalk_flag(" atom "," nonvar ")."
      | ":- include(" source_file_name ")."
      | ":- uses(" object_alias_list ")."
      | ":- use_module(" module_alias_list ")."
      | conditional_compilation_directive
      | predicate_directive

.. _grammar_category_directives:

Category directives
~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   category_directive ::=
      ":- built_in."
      | ":- dynamic."
      | ":- info(" entity_info_list ")."
      | ":- set_logtalk_flag(" atom "," nonvar ")."
      | ":- include(" source_file_name ")."
      | ":- uses(" object_alias_list ")."
      | ":- use_module(" module_alias_list ")."
      | conditional_compilation_directive
      | predicate_directive

.. _grammar_protocol_directives:

Protocol directives
~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   protocol_directive ::=
      ":- built_in."
      | ":- dynamic."
      | ":- info(" entity_info_list ")."
      | ":- set_logtalk_flag(" atom "," nonvar ")."
      | ":- include(" source_file_name ")."
      | conditional_compilation_directive
      | predicate_directive

.. _grammar_predicate_directives:

Predicate directives
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

   predicate_directive ::=
      alias_directive
      | synchronized_directive
      | uses_directive
      | use_module_directive
      | scope_directive
      | mode_directive
      | meta_predicate_directive
      | meta_non_terminal_directive
      | info_directive
      | dynamic_directive
      | discontiguous_directive
      | multifile_directive
      | coinductive_directive
      | operator_directive

   alias_directive ::=
      ":- alias(" entity_identifier "," alias_directive_resource_list ")."

   synchronized_directive ::=
      ":- synchronized(" synchronized_directive_resource_term ")."

   uses_directive ::=
      ":- uses(" ( object_identifier | parameter_variable ) "," uses_directive_resource_list ")."

   use_module_directive ::=
      ":- use_module(" ( module_identifier | parameter_variable ) "," use_module_directive_resource_list ")."

   scope_directive ::=
      ":- public(" scope_directive_resource_term ")."
      | ":- protected(" scope_directive_resource_term ")."
      | ":- private(" scope_directive_resource_term ")."

   mode_directive ::=
      ":- mode(" ( predicate_mode_term | non_terminal_mode_term ) "," number_of_proofs ")."

   meta_predicate_directive ::=
      ":- meta_predicate(" meta_predicate_template_term ")."

   meta_non_terminal_directive ::=
      ":- meta_non_terminal(" meta_non_terminal_template_term ")."

   info_directive ::=
      ":- info(" ( predicate_indicator | non_terminal_indicator ) "," predicate_info_list ")."

   dynamic_directive ::=
      ":- dynamic(" qualified_directive_resource_term ")."

   discontiguous_directive ::=
      ":- discontiguous(" qualified_directive_resource_term ")."

   multifile_directive ::=
      ":- multifile(" qualified_directive_resource_term ")."

   coinductive_directive ::=
      ":- coinductive(" ( predicate_indicator_term | coinductive_predicate_template_term ) ")."

   parameter_variable ::=
      _variable_

   scope_directive_resource_term ::=
      scope_directive_resource
      | scope_directive_resource_sequence
      | scope_directive_resource_list

   scope_directive_resource ::=
      predicate_indicator
      | non_terminal_indicator
      | operator

   scope_directive_resource_sequence ::=
      scope_directive_resource
      | scope_directive_resource "," scope_directive_resource_sequence

   scope_directive_resource_list ::=
      "[" scope_directive_resource_sequence "]"

   entity_resources_list ::=
      predicate_indicator_list
      | operator_list

   predicate_indicator_term ::=
      predicate_indicator
      | predicate_indicator_sequence
      | predicate_indicator_list

   predicate_indicator_sequence ::=
      predicate_indicator
      | predicate_indicator "," predicate_indicator_sequence

   predicate_indicator_list ::=
      "[" predicate_indicator_sequence "]"

   alias_directive_resource_list ::=
      "[" alias_directive_resource_sequence "]"

   alias_directive_resource_sequence ::=
      alias_directive_resource
      | alias_directive_resource "," alias_directive_resource_sequence

   alias_directive_resource ::=
      predicate_indicator_alias
      | non_terminal_indicator_alias

   synchronized_directive_resource_term ::=
      synchronized_directive_resource
      | synchronized_directive_resource_sequence
      | synchronized_directive_resource_list

   synchronized_directive_resource ::=
      predicate_indicator
      | non_terminal_indicator

   synchronized_directive_resource_sequence ::=
      synchronized_directive_resource
      | synchronized_directive_resource "," synchronized_directive_resource_sequence

   synchronized_directive_resource_list ::=
      "[" synchronized_directive_resource_sequence "]"

   uses_directive_resource_list ::=
      "[" uses_directive_resource_sequence "]"

   uses_directive_resource_sequence ::=
      uses_directive_resource
      | uses_directive_resource "," uses_directive_resource_sequence

   uses_directive_resource ::=
      predicate_indicator
      | non_terminal_indicator
      | predicate_template_alias
      | operator

   use_module_directive_resource_list ::=
      "[" use_module_directive_resource_sequence "]"

   use_module_directive_resource_sequence ::=
      use_module_directive_resource
      | use_module_directive_resource "," use_module_directive_resource_sequence

   use_module_directive_resource ::=
      predicate_indicator
      | non_terminal_indicator
      | predicate_template_alias
      | operator

   qualified_directive_resource_term ::=
      qualified_directive_resource
      | qualified_directive_resource_sequence
      | qualified_directive_resource_list

   qualified_directive_resource_sequence ::=
      qualified_directive_resource
      | qualified_directive_resource "," qualified_directive_resource_sequence

   qualified_directive_resource_list ::=
      "[" qualified_directive_resource_sequence "]"

   qualified_directive_resource ::=
      predicate_indicator
      | non_terminal_indicator
      | object_identifier "::" ( predicate_indicator | non_terminal_indicator)
      | category_identifier "::" ( predicate_indicator | non_terminal_indicator)
      | module_identifier ":" ( predicate_indicator | non_terminal_indicator)

   predicate_indicator_alias ::=
      predicate_indicator "as" predicate_indicator

   predicate_template_alias ::=
      callable "as" callable

   non_terminal_indicator ::=
      functor "//" arity

   non_terminal_indicator_alias ::=
      non_terminal_indicator "as" non_terminal_indicator

   operator_sequence ::=
      operator specification
      | operator specification "," operator_sequence

   operator_list ::=
      "[" operator_sequence "]"

   coinductive_predicate_template_term ::=
      coinductive_predicate_template
      | coinductive_predicate_template_sequence
      | coinductive_predicate_template_list

   coinductive_predicate_template_sequence ::=
      coinductive_predicate_template
      | coinductive_predicate_template "," coinductive_predicate_template_sequence

   coinductive_predicate_template_list ::=
      "[" coinductive_predicate_template_sequence "]"

   coinductive_predicate_template ::=
      atom "(" coinductive_mode_terms ")"

   coinductive_mode_terms ::=
      coinductive_mode_term
      | coinductive_mode_terms "," coinductive_mode_terms

   coinductive_mode_term ::=
      "+"
      | "-"

   predicate_mode_term ::=
      atom "(" mode_terms ")"

   non_terminal_mode_term ::=
      atom "(" mode_terms ")"

   mode_terms ::=
      mode_term
      |mode_term "," mode_terms

   mode_term ::=
      "@"  type?
      | "+" type?
      | "-" type?
      | "?" type?
      | "++" type?
      | "--" type?

   type ::=
      prolog_type | logtalk_type | user_defined_type

   prolog_type ::=
      "term"
      | "nonvar"
      | "var"
      | "compound"
      | "ground"
      | "callable"
      | "list"
      | "atomic"
      | "atom"
      | "number"
      | "integer"
      | "float"

   logtalk_type ::=
      "object"
      | "category"
      | "protocol"
      | "event"

   user_defined_type ::=
      atom
      | compound

   number_of_proofs ::=
      "zero"
      | "zero_or_one"
      | "zero_or_more"
      | "one"
      | "one_or_more"
      | "zero_or_error"
      | "one_or_error"
      | "zero_or_one_or_error"
      | "zero_or_more_or_error"
      | "one_or_more_or_error"
      | "error"

   meta_predicate_template_term ::=
      meta_predicate_template
      | meta_predicate_template_sequence
      | meta_predicate_template_list

   meta_predicate_template_sequence ::=
      meta_predicate_template
      | meta_predicate_template "," meta_predicate_template_sequence

   meta_predicate_template_list ::=
      "[" meta_predicate_template_sequence "]"

   meta_predicate_template ::=
      object_identifier "::" atom "(" meta_predicate_specifiers ")"
      | category_identifier "::" atom "(" meta_predicate_specifiers ")"
      | module_identifier ":" atom "(" meta_predicate_specifiers ")"
      | atom "(" meta_predicate_specifiers ")"

   meta_predicate_specifiers ::=
      meta_predicate_specifier
      | meta_predicate_specifier "," meta_predicate_specifiers

   meta_predicate_specifier ::=
      non_negative_integer
      | "::"
      | "^"
      | "*"

   meta_non_terminal_template_term ::=
      meta_predicate_template_term

   entity_info_list ::=
      "[" entity_info_sequence? "]"

   entity_info_sequence ::=
      entity_info_item "is" nonvar
      | entity_info_item "is" nonvar "," entity_info_sequence

   entity_info_item ::=
      "comment"
      | "remarks"
      | "author"
      | "version"
      | "date"
      | "copyright"
      | "license"
      | "parameters"
      | "parnames"
      | "see_also"
      | atom

   predicate_info_list ::=
      "[" predicate_info_sequence? "]"

   predicate_info_sequence ::=
      predicate_info_item "is" nonvar
      | predicate_info_item "is" nonvar "," predicate_info_sequence

   predicate_info_item ::=
      "comment"
      | "remarks"
      | "arguments"
      | "argnames"
      | "redefinition"
      | "allocation"
      | "examples"
      | "exceptions"
      | "see_also"
      | atom

   object_alias_list ::=
      "[" object_alias_sequence "]"

   object_alias_sequence ::=
      object_alias
      | object_alias "," object_alias_sequence

   object_alias ::=
      object_identifier "as" object_identifier

   module_alias_list ::=
      "[" module_alias_sequence "]"

   module_alias_sequence ::=
      module_alias
      | module_alias "," module_alias_sequence

   module_alias ::=
      module_identifier "as" module_identifier

.. _grammar_clauses:

Clauses and goals
-----------------

.. code-block:: bnf

   clause ::=
      object_identifier "::" head ":-" body
      | module_identifier ":" head ":-" body
      | head ":-" body
      | object_identifier "::" fact
      | module_identifier ":" fact
      | fact

   goal ::=
      message_sending
      | super_call
      | external_call
      | context_switching_call
      | callable

   message_sending ::=
      message_to_object
      | message_delegation
      | message_to_self

   message_to_object ::=
      receiver "::" messages

   message_delegation ::=
      "[" message_to_object "]"

   message_to_self ::=
      "::" messages

   super_call ::=
      "^^" message

   messages ::=
      message
      | "(" message "," messages ")"
      | "(" message ";" messages ")"
      | "(" message "->" messages ")"

   message ::=
      callable
      | variable

   receiver ::=
      "{" callable "}"
      | object_identifier
      | variable

   external_call ::=
      "{" callable "}"

   context_switching_call ::=
      object_identifier "<<" callable

.. _grammar_lambdas:

Lambda expressions
------------------

.. code-block:: bnf

   lambda_expression ::=
      lambda_free_variables "/" lambda_parameters ">>" callable
      | lambda_free_variables "/" callable
      | lambda_parameters ">>" callable

   lambda_free_variables ::=
      "{" variables? "}"

   lambda_parameters ::=
      "[" terms? "]"

   variables ::=
      variable
      | variable "," variables

   terms ::=
      term
      | term "," terms

.. _grammar_entity_properties:

Entity properties
-----------------

.. code-block:: bnf

   category_property ::=
      "static"
      | "dynamic"
      | "built_in"
      | "file(" atom ")"
      | "file(" atom "," atom ")"
      | "lines(" integer "," integer ")"
      | "directive(" integer "," integer ")"
      | "events"
      | "source_data"
      | "public(" entity_resources_list ")"
      | "protected(" entity_resources_list ")"
      | "private(" entity_resources_list ")"
      | "declares(" predicate_indicator "," predicate_declaration_property_list ")"
      | "defines(" predicate_indicator "," predicate_definition_property_list ")"
      | "includes(" predicate_indicator "," ( object_identifier | category_identifier ) "," predicate_definition_property_list ")"
      | "provides(" predicate_indicator "," ( object_identifier | category_identifier ) "," predicate_definition_property_list ")"
      | "alias(" ( object_identifier | module_identifier ) "," entity_alias_property_list ")"
      | "alias(" predicate_indicator "," predicate_alias_property_list ")"
      | "calls(" predicate "," predicate_call_update_property_list ")"
      | "updates(" predicate "," predicate_call_update_property_list ")"
      | "number_of_clauses(" integer ")"
      | "number_of_rules(" integer ")"
      | "number_of_user_clauses(" integer ")"
      | "number_of_user_rules(" integer ")"
      | "debugging"

   object_property ::=
      "static"
      | "dynamic"
      | "built_in"
      | "threaded"
      | "file(" atom ")"
      | "file(" atom "," atom ")"
      | "lines(" integer "," integer ")"
      | "directive(" integer "," integer ")"
      | "context_switching_calls"
      | "dynamic_declarations"
      | "events"
      | "source_data"
      | "complements(" ( "allow" | "restrict" ) ")"
      | "complements"
      | "public(" entity_resources_list ")"
      | "protected(" entity_resources_list ")"
      | "private(" entity_resources_list ")"
      | "declares(" predicate_indicator "," predicate_declaration_property_list ")"
      | "defines(" predicate_indicator "," predicate_definition_property_list ")"
      | "includes(" predicate_indicator "," ( object_identifier | category_identifier ) "," predicate_definition_property_list ")"
      | "provides(" predicate_indicator "," ( object_identifier | category_identifier ) "," predicate_definition_property_list ")"
      | "alias(" ( object_identifier | module_identifier ) "," entity_alias_property_list ")"
      | "alias(" predicate_indicator "," predicate_alias_property_list ")"
      | "calls(" predicate "," predicate_call_update_property_list ")"
      | "updates(" predicate "," predicate_call_update_property_list ")"
      | "number_of_clauses(" integer ")"
      | "number_of_rules(" integer ")"
      | "number_of_user_clauses(" integer ")"
      | "number_of_user_rules(" integer ")"
      | "module"
      | "debugging"

   protocol_property ::=
      "static"
      | "dynamic"
      | "built_in"
      | "source_data"
      | "file(" atom ")"
      | "file(" atom "," atom ")"
      | "lines(" integer "," integer ")"
      | "directive(" integer "," integer ")"
      | "public(" entity_resources_list ")"
      | "protected(" entity_resources_list ")"
      | "private(" entity_resources_list ")"
      | "declares(" predicate_indicator "," predicate_declaration_property_list ")"
      | "alias(" predicate_indicator "," predicate_alias_property_list ")"
      | "debugging"

   predicate_declaration_property_list ::=
      "[" predicate_declaration_property_sequence "]"

   predicate_declaration_property_sequence ::=
      predicate_declaration_property
      | predicate_declaration_property "," predicate_declaration_property_sequence

   predicate_declaration_property ::=
      "static"
      | "dynamic"
      | "scope(" scope ")"
      | "private"
      | "protected"
      | "public"
      | "coinductive"
      | "multifile"
      | "synchronized"
      | "meta_predicate(" meta_predicate_template ")"
      | "coinductive(" coinductive_predicate_template ")"
      | "non_terminal(" non_terminal_indicator ")"
      | "include(" atom ")"
      | "lines(" integer  "," integer ")"
      | "line_count(" integer ")"
      | "mode(" ( predicate_mode_term | non_terminal_mode_term ) "," number_of_proofs ")"
      | "info(" list ")"

   predicate_definition_property_list ::=
      "[" predicate_definition_property_sequence "]"

   predicate_definition_property_sequence ::=
      predicate_definition_property
      | predicate_definition_property "," predicate_definition_property_sequence

   predicate_definition_property ::=
      "inline"
      | "auxiliary"
      | "non_terminal(" non_terminal_indicator ")"
      | "include(" atom ")"
      | "lines(" integer  "," integer ")"
      | "line_count(" integer ")"
      | "number_of_clauses(" integer ")"
      | "number_of_rules(" integer ")"

   entity_alias_property_list ::=
      "[" entity_alias_property_sequence "]"

   entity_alias_property_sequence ::=
      entity_alias_property
      | entity_alias_property "," entity_alias_property_sequence

   entity_alias_property ::=
      "object"
      | "module"
      | "for(" ( object_identifier | module_identifier ) ")"
      | "include(" atom ")"
      | "lines(" integer  "," integer ")"
      | "line_count(" integer ")"

   predicate_alias_property_list ::=
      "[" predicate_alias_property_sequence "]"

   predicate_alias_property_sequence ::=
      predicate_alias_property
      | predicate_alias_property "," predicate_alias_property_sequence

   predicate_alias_property ::=
      "predicate"
      | "for(" predicate_indicator ")"
      | "from(" entity_identifier ")"
      | "non_terminal(" non_terminal_indicator ")"
      | "include(" atom ")"
      | "lines(" integer  "," integer ")"
      | "line_count(" integer ")"

   predicate ::=
      predicate_indicator
      | "^^" predicate_indicator
      | "::" predicate_indicator
      | ( variable | object_identifier ) "::" predicate_indicator
      | ( variable | module_identifier ) ":" predicate_indicator

   predicate_call_update_property_list ::=
      "[" predicate_call_update_property_sequence "]"

   predicate_call_update_property_sequence ::=
      predicate_call_update_property
      | predicate_call_update_property "," predicate_call_update_property_sequence

   predicate_call_update_property ::=
      "caller(" predicate_indicator ")"
      | "include(" atom ")"
      | "lines(" integer  "," integer ")"
      | "line_count(" integer ")"
      | "alias(" predicate_indicator ")"
      | "non_terminal(" non_terminal_indicator ")"

.. _grammar_predicate_properties:

Predicate properties
--------------------

.. code-block:: bnf

   predicate_property ::=
      "static"
      | "dynamic"
      | "scope(" scope ")"
      | "private"
      | "protected"
      | "public"
      | "logtalk"
      | "prolog"
      | "foreign"
      | "coinductive(" coinductive_predicate_template ")"
      | "multifile"
      | "synchronized"
      | "built_in"
      | "inline"
      | "recursive"
      | "declared_in(" entity_identifier ")"
      | "defined_in(" ( object_identifier | category_identifier ) ")"
      | "redefined_from(" ( object_identifier | category_identifier ) ")"
      | "meta_predicate(" meta_predicate_template ")"
      | "alias_of(" callable ")"
      | "alias_declared_in(" entity_identifier ")"
      | "non_terminal(" non_terminal_indicator ")"
      | "mode(" ( predicate_mode_term | non_terminal_mode_term ) "," number_of_proofs ")"
      | "info(" list ")"
      | "number_of_clauses(" integer ")"
      | "number_of_rules(" integer ")"
      | "declared_in(" entity_identifier "," line_count ")"
      | "defined_in(" ( object_identifier | category_identifier ) "," line_count ")"
      | "redefined_from(" ( object_identifier | category_identifier ) "," line_count ")"
      | "alias_declared_in(" entity_identifier "," line_count ")"

   line_count ::=
      integer

.. _grammar_compiler_flags:

Compiler flags
--------------

.. code-block:: bnf

   compiler_flag ::=
      flag "(" flag_value ")"
