..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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

The Logtalk grammar is here described using Backus-Naur Form syntax.
Non-terminal symbols in *italics* have the definition found in the ISO
Prolog Core standard. Terminal symbols are represented in a
``fixed width font`` and between double-quotes.

.. _grammar_entities:

Entities
--------

| entity ::=
|    object \|
|    category \|
|    protocol

.. _grammar_object_definition:

Object definition
-----------------

| object ::=
|    begin_object_directive [ object_terms ] end_object_directive.

| begin_object_directive ::=
|    "``:- object(``" object_identifier [ "``,``" object_relations ] "``).``"

| end_object_directive ::=
|    "``:- end_object.``"

| object_relations ::=
|    prototype_relations \|
|    non_prototype_relations

| prototype_relations ::=
|    prototype_relation \|
|    prototype_relation "``,``" prototype_relations

| prototype_relation ::=
|    implements_protocols \|
|    imports_categories \|
|    extends_objects

| non_prototype_relations ::=
|    non_prototype_relation \|
|    non_prototype_relation "``,``" non_prototype_relations

| non_prototype_relation ::=
|    implements_protocols \|
|    imports_categories \|
|    instantiates_classes \|
|    specializes_classes

.. _grammar_category_definition:

Category definition
-------------------

| category ::=
|    begin_category_directive [ category_terms ] end_category_directive.

| begin_category_directive ::=
|    "``:- category(``" category_identifier [ "``,``" category_relations ] "``).``"

| end_category_directive ::=
|    "``:- end_category.``"

| category_relations ::=
|    category_relation \|
|    category_relation "``,``" category_relations

| category_relation ::=
|    implements_protocols \|
|    extends_categories \|
|    complements_objects

.. _grammar_protocol_definition:

Protocol definition
-------------------

| protocol ::=
|    begin_protocol_directive [ protocol_directives ] end_protocol_directive.

| begin_protocol_directive ::=
|    "``:- protocol(``" protocol_identifier [ "``,``" extends_protocols ] "``).``"

| end_protocol_directive ::=
|    "``:- end_protocol.``"

.. _grammar_entity_relations:

Entity relations
----------------

| extends_protocols ::=
|    "``extends(``" extended_protocols "``)``"

| extends_objects ::=
|    "``extends(``" extended_objects "``)``"

| extends_categories ::=
|    "``extends(``" extended_categories "``)``"

| implements_protocols ::=
|    "``implements(``" implemented_protocols "``)``"

| imports_categories ::=
|    "``imports(``" imported_categories "``)``"

| instantiates_classes ::=
|    "``instantiates(``" instantiated_objects "``)``"

| specializes_classes ::=
|    "``specializes(``" specialized_objects "``)``"

| complements_objects ::=
|    "``complements(``" complemented_objects "``)``"

.. _grammar_implemented_protocols:

Implemented protocols
~~~~~~~~~~~~~~~~~~~~~

| implemented_protocols ::=
|    implemented_protocol \|
|    implemented_protocol_sequence \|
|    implemented_protocol_list

| implemented_protocol ::=
|    protocol_identifier \|
|    scope "``::``" protocol_identifier

| implemented_protocol_sequence ::=
|    implemented_protocol \|
|    implemented_protocol "``,``" implemented_protocol_sequence

| implemented_protocol_list ::=
|    "``[``" implemented_protocol_sequence "``]``"

.. _grammar_extended_protocols:

Extended protocols
~~~~~~~~~~~~~~~~~~

| extended_protocols ::=
|    extended_protocol \|
|    extended_protocol_sequence \|
|    extended_protocol_list

| extended_protocol ::=
|    protocol_identifier \|
|    scope "``::``" protocol_identifier

| extended_protocol_sequence ::=
|    extended_protocol \|
|    extended_protocol "``,``" extended_protocol_sequence

| extended_protocol_list ::=
|    "``[``" extended_protocol_sequence "``]``"

.. _grammar_imported_categories:

Imported categories
~~~~~~~~~~~~~~~~~~~

| imported_categories ::=
|    imported_category \|
|    imported_category_sequence \|
|    imported_category_list

| imported_category ::=
|    category_identifier \|
|    scope "``::``" category_identifier

| imported_category_sequence ::=
|    imported_category \|
|    imported_category "``,``" imported_category_sequence

| imported_category_list ::=
|    "``[``" imported_category_sequence "``]``"

.. _grammar_extended_objects:

Extended objects
~~~~~~~~~~~~~~~~

| extended_objects ::=
|    extended_object \|
|    extended_object_sequence \|
|    extended_object_list

| extended_object ::=
|    object_identifier \|
|    scope "``::``" object_identifier

| extended_object_sequence ::=
|    extended_object \|
|    extended_object "``,``" extended_object_sequence

| extended_object_list ::=
|    "``[``" extended_object_sequence "``]``"

.. _grammar_extended_categories:

Extended categories
~~~~~~~~~~~~~~~~~~~

| extended_categories ::=
|    extended_category \|
|    extended_category_sequence \|
|    extended_category_list

| extended_category ::=
|    category_identifier \|
|    scope "``::``" category_identifier

| extended_category_sequence ::=
|    extended_category \|
|    extended_category "``,``" extended_category_sequence

| extended_category_list ::=
|    "``[``" extended_category_sequence "``]``"

.. _grammar_instantiated_objects:

Instantiated objects
~~~~~~~~~~~~~~~~~~~~

| instantiated_objects ::=
|    instantiated_object \|
|    instantiated_object_sequence \|
|    instantiated_object_list

| instantiated_object ::=
|    object_identifier \|
|    scope "``::``" object_identifier

| instantiated_object_sequence ::=
|    instantiated_object
|    instantiated_object "``,``" instantiated_object_sequence \|

| instantiated_object_list ::=
|    "``[``" instantiated_object_sequence "``]``"

.. _grammar_specialized_objects:

Specialized objects
~~~~~~~~~~~~~~~~~~~

| specialized_objects ::=
|    specialized_object \|
|    specialized_object_sequence \|
|    specialized_object_list

| specialized_object ::=
|    object_identifier \|
|    scope "``::``" object_identifier

| specialized_object_sequence ::=
|    specialized_object \|
|    specialized_object "``,``" specialized_object_sequence

| specialized_object_list ::=
|    "``[``" specialized_object_sequence "``]``"

.. _grammar_complemented_objects:

Complemented objects
~~~~~~~~~~~~~~~~~~~~

| complemented_objects ::=
|    object_identifier \|
|    complemented_object_sequence \|
|    complemented_object_list

| complemented_object_sequence ::=
|    object_identifier \|
|    object_identifier "``,``" complemented_object_sequence

| complemented_object_list ::=
|    "``[``" complemented_object_sequence "``]``"

.. _grammar_scope:

Entity and predicate scope
~~~~~~~~~~~~~~~~~~~~~~~~~~

| scope ::=
|    "``public``" \|
|    "``protected``" \|
|    "``private``"

.. _grammar_entity_identifiers:

Entity identifiers
------------------

| entity_identifiers ::=
|    entity_identifier \|
|    entity_identifier_sequence \|
|    entity_identifier_list

| entity_identifier ::=
|    object_identifier \|
|    protocol_identifier \|
|    category_identifier

| entity_identifier_sequence ::=
|    entity_identifier \|
|    entity_identifier "``,``" entity_identifier_sequence

| entity_identifier_list ::=
|    "``[``" entity_identifier_sequence "``]``"

.. _grammar_object_identifiers:

Object identifiers
~~~~~~~~~~~~~~~~~~

| object_identifiers ::=
|    object_identifier \|
|    object_identifier_sequence \|
|    object_identifier_list

| object_identifier ::=
|    *atom* \|
|    *compound*

| object_identifier_sequence ::=
|    object_identifier \|
|    object_identifier "``,``" object_identifier_sequence

| object_identifier_list ::=
|    "``[``" object_identifier_sequence "``]``"

.. _grammar_category_identifiers:

Category identifiers
~~~~~~~~~~~~~~~~~~~~

| category_identifiers ::=
|    category_identifier \|
|    category_identifier_sequence \|
|    category_identifier_list

| category_identifier ::=
|    *atom* \|
|    *compound*

| category_identifier_sequence ::=
|    category_identifier \|
|    category_identifier "``,``" category_identifier_sequence

| category_identifier_list ::=
|    "``[``" category_identifier_sequence "``]``"

.. _grammar_protocol_identifiers:

Protocol identifiers
~~~~~~~~~~~~~~~~~~~~

| protocol_identifiers ::=
|    protocol_identifier \|
|    protocol_identifier_sequence \|
|    protocol_identifier_list
| 
| protocol_identifier ::=
|    *atom*

| protocol_identifier_sequence ::=
|    protocol_identifier \|
|    protocol_identifier "``,``" protocol_identifier_sequence

| protocol_identifier_list ::=
|    "``[``" protocol_identifier_sequence "``]``"

.. _grammar_module_identifiers:

Module identifiers
~~~~~~~~~~~~~~~~~~

| module_identifier ::=
|    *atom*

.. _grammar_source_file_names:

Source file names
-----------------

| source_file_names ::=
|    source_file_name \|
|    source_file_name_list

| source_file_name ::=
|    *atom* \|
|    library_source_file_name

| library_source_file_name ::=
|    library_name "``(``" *atom* "``)``"

| library_name ::=
|    *atom*

| source_file_name_sequence ::=
|    source_file_name \|
|    source_file_name "``,``" source_file_name_sequence

| source_file_name_list ::=
|    "``[``" source_file_name_sequence "``]``"

.. _grammar_terms:

Terms
-----

.. _grammar_object_terms:

Object terms
~~~~~~~~~~~~

| object_terms ::=
|    object_term \|
|    object_term object_terms

| object_term ::=
|    object_directive \|
|    clause \|
|    grammar_rule

.. _grammar_category_terms:

Category terms
~~~~~~~~~~~~~~

| category_terms ::=
|    category_term \|
|    category_term category_terms

| category_term ::=
|    category_directive \|
|    clause \|
|    grammar_rule

.. _grammar_directives:

Directives
----------

.. _grammar_source_file_directives:

Source file directives
~~~~~~~~~~~~~~~~~~~~~~

| source_file_directives ::=
|    source_file_directive \|
|    source_file_directive source_file_directives

| source_file_directive ::=
|    "``:- encoding(``" *atom* "``).``" \|
|    "``:- set_logtalk_flag(``" *atom* "``,``" *nonvar* "``).``" \|
|    "``:- include(``" source_file_name "``).``"
|    *Prolog directives*

.. _grammar_conditional_compilation_directives:

Conditional compilation directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| conditional_compilation_directives ::=
|    conditional_compilation_directive \|
|    conditional_compilation_directive conditional_compilation_directives

| conditional_compilation_directive ::=
|    "``:- if(``" *callable* "``).``" \|
|    "``:- elif(``" *callable* "``).``" \|
|    "``:- else.``" \|
|    "``:- endif.``"

.. _grammar_object_directives:

Object directives
~~~~~~~~~~~~~~~~~

| object_directives ::=
|    object_directive \|
|    object_directive object_directives

| object_directive ::=
|    "``:- initialization(``" *callable* "``).``" \|
|    "``:- built_in.``" \|
|    "``:- threaded.``" \|
|    "``:- dynamic.``" \|
|    "``:- info(``" entity_info_list "``).``" \|
|    "``:- set_logtalk_flag(``" *atom* "``,``" *nonvar* "``).``" \|
|    "``:- include(``" source_file_name "``).``" \|
|    "``:- uses(``" object_alias_list "``).``" \|
|    predicate_directives

.. _grammar_category_directives:

Category directives
~~~~~~~~~~~~~~~~~~~

| category_directives ::=
|    category_directive \|
|    category_directive category_directives

| category_directive ::=
|    "``:- built_in.``" \|
|    "``:- dynamic.``" \|
|    "``:- info(``" entity_info_list "``).``" \|
|    "``:- set_logtalk_flag(``" *atom* "``,``" *nonvar* "``).``" \|
|    "``:- include(``" source_file_name "``).``" \|
|    "``:- uses(``" object_alias_list "``).``" \|
|    predicate_directives

.. _grammar_protocol_directives:

Protocol directives
~~~~~~~~~~~~~~~~~~~

| protocol_directives ::=
|    protocol_directive \|
|    protocol_directive protocol_directives

| protocol_directive ::=
|    "``:- built_in.``" \|
|    "``:- dynamic.``" \|
|    "``:- info(``" entity_info_list "``).``" \|
|    "``:- set_logtalk_flag(``" *atom* "``,``" *nonvar* "``).``" \|
|    "``:- include(``" source_file_name "``).``" \|
|    predicate_directives

.. _grammar_predicate_directives:

Predicate directives
~~~~~~~~~~~~~~~~~~~~

| predicate_directives ::=
|    predicate_directive \|
|    predicate_directive predicate_directives

| predicate_directive ::=
|    alias_directive \|
|    synchronized_directive \|
|    uses_directive \|
|    use_module_directive \|
|    scope_directive \|
|    mode_directive \|
|    meta_predicate_directive \|
|    meta_non_terminal_directive \|
|    info_directive \|
|    dynamic_directive \|
|    discontiguous_directive \|
|    multifile_directive \|
|    coinductive_directive \|
|    *operator_directive*

| alias_directive ::=
|    "``:- alias(``"
|           entity_identifier "``,``"
|           predicate_indicator_alias_list \| non_terminal_indicator_alias_list
|        "``).``"

| synchronized_directive ::=
|    "``:- synchronized(``" predicate_indicator_term \| non_terminal_indicator_term "``).``"

| uses_directive ::=
|    "``:- uses(``"
|           object_identifier \| parameter_variable "``,``"
|           predicate_indicator_alias_list \| non_terminal_indicator_alias_list \| operator_list
|    "``).``"

| use_module_directive ::=
|    "``:- use_module(``"
|           module_identifier \| parameter_variable "``,``"
|           module_predicate_indicator_alias_list \| module_non_terminal_indicator_alias_list \| operator_list
|    "``).``"

| scope_directive ::=
|    "``:- public(``" predicate_indicator_term \| non_terminal_indicator_term "``).``" \|
|    "``:- protected(``" predicate_indicator_term \| non_terminal_indicator_term "``).``" \|
|    "``:- private(``" predicate_indicator_term \| non_terminal_indicator_term "``).``"

| mode_directive ::=
|    "``:- mode(``"
|           predicate_mode_term \| non_terminal_mode_term "``,``"
|           number_of_proofs
|    "``).``"

| meta_predicate_directive ::=
|    "``:- meta_predicate(``" meta_predicate_template_term "``).``"

| meta_non_terminal_directive ::=
|    "``:- meta_non_terminal(``" meta_non_terminal_template_term "``).``"

| info_directive ::=
|    "``:- info(``"
|           predicate_indicator \| non_terminal_indicator "``,``"
|           predicate_info_list
|    "``).``"

| dynamic_directive ::=
|    "``:- dynamic(``" qualified_predicate_indicator_term \| qualified_non_terminal_indicator_term "``).``"

| discontiguous_directive ::=
|    "``:- discontiguous(``" predicate_indicator_term \|
|    non_terminal_indicator_term "``).``"

| multifile_directive ::=
|    "``:- multifile(``" qualified_predicate_indicator_term \|
|    qualified_non_terminal_indicator_term "``).``"

| coinductive_directive ::=
|    "``:- coinductive(``" predicate_indicator_term \|
|    coinductive_predicate_template_term "``).``"

| parameter_variable ::=
|    *_variable_*

| predicate_indicator_term ::=
|    *predicate_indicator* \|
|    predicate_indicator_sequence \|
|    predicate_indicator_list

| predicate_indicator_sequence ::=
|    *predicate_indicator* \|
|    *predicate_indicator* "``,``" predicate_indicator_sequence

| predicate_indicator_list ::=
|    "``[``" predicate_indicator_sequence "``]``"

| qualified_predicate_indicator_term ::=
|    qualified_predicate_indicator \|
|    qualified_predicate_indicator_sequence \|
|    qualified_predicate_indicator_list

| qualified_predicate_indicator_sequence ::=
|    qualified_predicate_indicator \|
|    qualified_predicate_indicator "``,``" qualified_predicate_indicator_sequence

| qualified_predicate_indicator_list ::=
|    "``[``" qualified_predicate_indicator_sequence "``]``"

| qualified_predicate_indicator ::=
|    predicate_indicator \|
|    object_identifier "``::``" predicate_indicator \|
|    category_identifier "``::``" predicate_indicator \|
|    module_identifier "``:``" predicate_indicator

| predicate_indicator_alias ::=
|    *predicate_indicator* \|
|    *predicate_indicator* "``as``" *predicate_indicator* \|
|    *predicate_indicator* "``::``" *predicate_indicator*

| predicate_indicator_alias_sequence ::=
|    predicate_indicator_alias \|
|    predicate_indicator_alias "``,``" predicate_indicator_alias_sequence

| predicate_indicator_alias_list ::=
|    "``[``" predicate_indicator_alias_sequence "``]``"

| predicate_template_alias ::=
|    *callable* "``as``" *callable* \|
|    *callable* "``::``" *callable*

| predicate_template_alias_sequence ::=
|    predicate_template_alias \|
|    predicate_template_alias "``,``" predicate_template_alias_sequence

| predicate_template_alias_list ::=
|    "``[``" predicate_template_alias_sequence "``]``"

| module_predicate_indicator_alias ::=
|    *predicate_indicator* \|
|    *predicate_indicator* "``as``" *predicate_indicator* \|
|    *predicate_indicator* "``:``" *predicate_indicator*

| module_predicate_indicator_alias_sequence ::=
|    module_predicate_indicator_alias \|
|    module_predicate_indicator_alias "``,``" module_predicate_indicator_alias_sequence

| module_predicate_indicator_alias_list ::=
|    "``[``" module_predicate_indicator_alias_sequence "``]``"

| module_non_terminal_indicator_alias ::=
|    *non_terminal_indicator* \|
|    *non_terminal_indicator* "``as``" *non_terminal_indicator*
|    *non_terminal_indicator* "``:``" *non_terminal_indicator*

| module_non_terminal_indicator_alias_sequence ::=
|    module_non_terminal_indicator_alias \|
|    module_non_terminal_indicator_alias "``,``" module_non_terminal_indicator_alias_sequence

| module_non_terminal_indicator_alias_list ::=
|    "``[``" module_non_terminal_indicator_alias_sequence "``]``"

| non_terminal_indicator_term ::=
|    non_terminal_indicator \|
|    non_terminal_indicator_sequence \|
|    non_terminal_indicator_list

| non_terminal_indicator_sequence ::=
|    non_terminal_indicator \|
|    non_terminal_indicator "``,``" non_terminal_indicator_sequence

| non_terminal_indicator_list ::=
|    "``[``" non_terminal_indicator_sequence "``]``"

| non_terminal_indicator ::=
|    *functor* "``//``" *arity*

| qualified_non_terminal_indicator_term ::=
|    qualified_non_terminal_indicator \|
|    qualified_non_terminal_indicator_sequence \|
|    qualified_non_terminal_indicator_list

| qualified_non_terminal_indicator_sequence ::=
|    qualified_non_terminal_indicator \|
|    qualified_non_terminal_indicator "``,`` " qualified_non_terminal_indicator_sequence

| qualified_non_terminal_indicator_list ::=
|    "``[``" qualified_non_terminal_indicator_sequence "``]``"

| qualified_non_terminal_indicator ::=
|    non_terminal_indicator \|
|    object_identifier "``::``" non_terminal_indicator \|
|    category_identifier "``::``" non_terminal_indicator \|
|    module_identifier "``:``" non_terminal_indicator

| non_terminal_indicator_alias ::=
|    *non_terminal_indicator* \|
|    *non_terminal_indicator* "``as``" *non_terminal_indicator*
|    *non_terminal_indicator* "``::``" *non_terminal_indicator*

| non_terminal_indicator_alias_sequence ::=
|    non_terminal_indicator_alias \|
|    non_terminal_indicator_alias "``,``"
|    non_terminal_indicator_alias_sequence

| non_terminal_indicator_alias_list ::=
|    "``[``" non_terminal_indicator_alias_sequence "``]``"

| operator_sequence ::=
|    *operator specification* \|
|    *operator specification* "``,``"
|    operator_sequence

| operator_list ::=
|    "``[``" operator_sequence "``]``"

| coinductive_predicate_template_term ::=
|    coinductive_predicate_template \|
|    coinductive_predicate_template_sequence \|
|    coinductive_predicate_template_list

| coinductive_predicate_template_sequence ::=
|    coinductive_predicate_template \|
|    coinductive_predicate_template "``,``"
|    coinductive_predicate_template_sequence

| coinductive_predicate_template_list ::=
|    "``[``" coinductive_predicate_template_sequence "``]``"

| coinductive_predicate_template ::=
|    *atom* "``(``" coinductive_mode_terms "``)``"

| coinductive_mode_terms ::=
|    coinductive_mode_term \|
|    coinductive_mode_terms "``,``" coinductive_mode_terms

| coinductive_mode_term ::=
|    "``+``" \| "``-``"

| predicate_mode_term ::=
|    *atom* "``(``" mode_terms "``)``"

| non_terminal_mode_term ::=
|    *atom* "``(``" mode_terms "``)``"

| mode_terms ::=
|    mode_term \|
|    mode_term "``,``" mode_terms

| mode_term ::=
|    "``@``" [ type ] \| "``+``" [ type ] \| "``-``" [ type ] \| "``?``" [
|    type ] \|
|    "``++``" [ type ] \| "``--``" [ type ]

| type ::=
|    prolog_type \| logtalk_type \| user_defined_type

| prolog_type ::=
|    "``term``" \| "``nonvar``" \| "``var``" \|
|    "``compound``" \| "``ground``" \| "``callable``" \| "``list``" \|
|    "``atomic``" \| "``atom``" \|
|    "``number``" \| "``integer``" \| "``float``"

| logtalk_type ::=
|    "``object``" \| "``category``" \| "``protocol``" \|
|    "``event``"

| user_defined_type ::=
|    *atom* \|
|    *compound*

| number_of_proofs ::=
|    "``zero``" \| "``zero_or_one``" \| "``zero_or_more``" \| "``one``" \|
|    "``one_or_more``" \| "``one_or_error``" \| "``error``"

| meta_predicate_template_term ::=
|    *meta_predicate_template* \|
|    meta_predicate_template_sequence \|
|    meta_predicate_template_list

| meta_predicate_template_sequence ::=
|    *meta_predicate_template* \|
|    *meta_predicate_template* "``,``" meta_predicate_template_sequence

| meta_predicate_template_list ::=
|    "``[``" meta_predicate_template_sequence "``]``"

| meta_predicate_template ::=
|    object_identifier "``::``" *atom* "``(``" meta_predicate_specifiers "``)``" \|
|    category_identifier "``::``" *atom* "``(``" meta_predicate_specifiers "``)``" \|
|    *atom* "``(``" meta_predicate_specifiers "``)``"

| meta_predicate_specifiers ::=
|    meta_predicate_specifier \|
|    meta_predicate_specifier "``,``" meta_predicate_specifiers

| meta_predicate_specifier ::=
|    *non-negative integer* \| "``::``" \| "``^``" \|
|    "``*``"

| meta_non_terminal_template_term ::=
|    meta_predicate_template_term

| entity_info_list ::=
|    "``[]``" \|
|    "``[``" entity_info_item "``is``" *nonvar* "``|``" entity_info_list
|    "``]``"

| entity_info_item ::=
|    "``comment``" \| "``remarks``" \|
|    "``author``" \| "``version``" \| "``date``" \|
|    "``copyright``" \| "``license``" \|
|    "``parameters``" \| "``parnames``" \|
|    "``see_also``" \|
|    *atom*

| predicate_info_list ::=
|    "``[]``" \|
|    "``[``" predicate_info_item "``is``" *nonvar* "``|``" predicate_info_list "``]``"

| predicate_info_item ::=
|    "``comment``" \| "``remarks``" \|
|    "``arguments``" \| "``argnames``" \|
|    "``redefinition``" \| "``allocation``" \|
|    "``examples``" \| "``exceptions``" \|
|    *atom*

| object_alias ::=
|    object_identifier "``as``" object_identifier

| object_alias_sequence ::=
|    object_alias \|
|    object_alias "``,``" object_alias_sequence

| object_alias_list ::=
|    "``[``" object_alias_sequence "``]``"

.. _grammar_clauses:

Clauses and goals
-----------------

| clause ::=
|    object_identifier "``::``" *head* "``:-``" *body* \|
|    module_identifier "``:``" *head* "``:-``" *body* \|
|    *head* :- *body* \|
|    *fact*

| goal ::=
|    message_sending \|
|    super_call \|
|    external_call \|
|    context_switching_call \|
|    *callable*

| message_sending ::=
|    message_to_object \|
|    message_delegation \|
|    message_to_self

| message_to_object ::=
|    receiver "``::``" messages

| message_delegation ::=
|    "``[``" message_to_object "``]``"

| message_to_self ::=
|    "``::``" messages

| super_call ::=
|    "``^^``" message

| messages ::=
|    message \|
|    "``(``" message "``,``" messages "``)``" \|
|    "``(``" message "``;``" messages "``)``" \|
|    "``(``" message "``->``" messages "``)``"

| message ::=
|    *callable* \|
|    *variable*

| receiver ::=
|    "``{``" *callable* "``}``" \|
|    object_identifier \|
|    *variable*

| external_call ::=
|    "``{``" *callable* "``}``"

| context_switching_call ::=
|    object_identifier "``<<``" *goal*

.. _grammar_lambdas:

Lambda expressions
------------------

| lambda_expression ::=
|    lambda_free_variables "``/``" lambda_parameters "``>>``" *callable* \|
|    lambda_free_variables "``/``" *callable* \|
|    lambda_parameters "``>>``" *callable*

| lambda_free_variables ::=
|    "``{``" *conjunction of variables* "``}``" \|
|    "``{``" *variable* "``}``" \|
|    "``{}``"

| lambda_parameters ::=
|    *list of terms* \|
|    "``[]``"

.. _grammar_entity_properties:

Entity properties
-----------------

| category_property ::=
|    "``static``" \|
|    "``dynamic``" \|
|    "``built_in``" \|
|    "``file(``" *atom* "``)``" \|
|    "``file(``" *atom* "``,``" *atom* "``)``" \|
|    "``lines(``" *integer* "``,``" *integer* "``)``" \|
|    "``events``" \|
|    "``source_data``" \|
|    "``public(``" predicate_indicator_list "``)``" \|
|    "``protected(``" predicate_indicator_list "``)``" \|
|    "``private(``" predicate_indicator_list "``)``" \|
|    "``declares(``" predicate_indicator "``,``" predicate_declaration_property_list "``)``" \|
|    "``defines(``" predicate_indicator "``,``" predicate_definition_property_list "``)``" \|
|    "``includes(``" predicate_indicator "``,``" object_identifier \| category_identifier "``,``" predicate_definition_property_list "``)``" \|
|    "``provides(``" predicate_indicator "``,``" object_identifier \| category_identifier "``,``" predicate_definition_property_list "``)``" \|
|    "``alias(``" predicate_indicator "``,``" predicate_alias_property_list "``)``" \|
|    "``calls(``" predicate "``,``" predicate_call_update_property_list "``)``" \|
|    "``updates(``" predicate "``,``" predicate_call_update_property_list "``)``" \|
|    "``number_of_clauses(``" *integer* "``)``" \|
|    "``number_of_rules(``" *integer* "``)``" \|
|    "``number_of_user_clauses(``" *integer* "``)``" \|
|    "``number_of_user_rules(``" *integer* "``)``" \|
|    "``debugging``"

| object_property ::=
|    "``static``" \|
|    "``dynamic``" \|
|    "``built_in``" \|
|    "``threaded``" \|
|    "``file(``" *atom* "``)``" \|
|    "``file(``" *atom* "``,``" *atom* "``)``" \|
|    "``lines(``" *integer* "``,``" *integer* "``)``" \|
|    "``context_switching_calls``" \|
|    "``dynamic_declarations``" \|
|    "``events``" \|
|    "``source_data``" \|
|    "``complements(``" "``allow``" \| "``restrict``" "``)``" \|
|    "``complements``" \|
|    "``public(``" predicate_indicator_list "``)``" \|
|    "``protected(``" predicate_indicator_list "``)``" \|
|    "``private(``" predicate_indicator_list "``)``" \|
|    "``declares(``" predicate_indicator "``,``" predicate_declaration_property_list "``)``" \|
|    "``defines(``" predicate_indicator "``,``" predicate_definition_property_list "``)``" \|
|    "``includes(``" predicate_indicator "``,``" object_identifier \| category_identifier "``,``" predicate_definition_property_list "``)``" \|
|    "``provides(``" predicate_indicator "``,``" object_identifier \| category_identifier "``,``" predicate_definition_property_list "``)``"
|    "``alias(``" predicate_indicator "``,``" predicate_alias_property_list "``)``" \|
|    "``calls(``" predicate "``,``" predicate_call_update_property_list "``)``" \|
|    "``updates(``" predicate "``,``" predicate_call_update_property_list "``)``" \|
|    "``number_of_clauses(``" *integer* "``)``" \|
|    "``number_of_rules(``" *integer* "``)``" \|
|    "``number_of_user_clauses(``" *integer* "``)``"
|    "``number_of_user_rules(``" *integer* "``)``" \|
|    "``module`` \|"
|    "``debugging``"

| protocol_property ::=
|    "``static``" \|
|    "``dynamic``" \|
|    "``built_in``" \|
|    "``source_data``" \|
|    "``file(``" *atom* "``)``" \|
|    "``file(``" *atom* "``,``" *atom* "``)``" \|
|    "``lines(``" *integer* "``,``" *integer* "``)``" \|
|    "``public(``" predicate_indicator_list "``)``" \|
|    "``protected(``" predicate_indicator_list "``)``" \|
|    "``private(``" predicate_indicator_list "``)``" \|
|    "``declares(``" predicate_indicator "``,``" predicate_declaration_property_list "``)``" \|
|    "``alias(``" predicate_indicator "``,``" predicate_alias_property_list "``)``" \|
|    "``debugging``"

| predicate_declaration_property_list ::=
|    "``[``" predicate_declaration_property_sequence "``]``"

| predicate_declaration_property_sequence ::=
|    predicate_declaration_property \|
|    predicate_declaration_property "``,``"
|    predicate_declaration_property_sequence

| predicate_declaration_property ::=
|    "``static``" \| "``dynamic``" \|
|    "``scope(``" scope "``)``" \|
|    "``private``" \| "``protected``" \| "``public``" \|
|    "``coinductive``" \|
|    "``multifile``" \|
|    "``synchronized``" \|
|    "``meta_predicate(``" meta_predicate_template "``)``" \|
|    "``coinductive(``" coinductive_predicate_template "``)``" \|
|    "``non_terminal(``" non_terminal_indicator "``)``" \|
|    "``include(``" *atom* "``)``" \|
|    "``line_count(``" *integer* "``)``" \|
|    "``mode(``" predicate_mode_term \| non_terminal_mode_term "``,``" number_of_proofs "``)``" \|
|    "``info(``" *list* "``)``"

| predicate_definition_property_list ::=
|    "``[``" predicate_definition_property_sequence "``]``"

| predicate_definition_property_sequence ::=
|    predicate_definition_property \|
|    predicate_definition_property "``,``"
|    predicate_definition_property_sequence

| predicate_definition_property ::=
|    "``inline``" \| "``auxiliary``" \|
|    "``non_terminal(``" non_terminal_indicator "``)``" \|
|    "``include(``" *atom* "``)``" \|
|    "``line_count(``" *integer* "``)``" \|
|    "``number_of_clauses(``" *integer* "``)``" \|
|    "``number_of_rules(``" *integer* "``)``"

| predicate_alias_property_list ::=
|    "``[``" predicate_alias_property_sequence "``]``"

| predicate_alias_property_sequence ::=
|    predicate_alias_property \|
|    predicate_alias_property "``,``" predicate_alias_property_sequence

| predicate_alias_property ::=
|    "``for(``" predicate_indicator "``)``" \|
|    "``from(``" entity_identifier "``)``" \|
|    "``non_terminal(``" non_terminal_indicator "``)``" \|
|    "``include(``" *atom* "``)``" \|
|    "``line_count(``" *integer* "``)``"

| predicate ::=
|    predicate_indicator \|
|    "``^^``" predicate_indicator \|
|    "``::``" predicate_indicator \|
|    *variable* "``::``" predicate_indicator \|
|    object_identifier "``::``" predicate_indicator \|
|    *variable* "``:``" predicate_indicator \|
|    module_identifier "``:``" predicate_indicator

| predicate_call_update_property_list ::=
|    "``[``" predicate_call_update_property_sequence "``]``"

| predicate_call_update_property_sequence ::=
|    predicate_call_update_property \|
|    predicate_call_update_property "``,``"
|    predicate_call_update_property_sequence

| predicate_call_update_property ::=
|    "``caller(``" predicate_indicator "``)``" \|
|    "``include(``" *atom* "``)``" \|
|    "``line_count(``" *integer* "``)``" \|
|    "``as(``" predicate_indicator "``)``"

.. _grammar_predicate_properties:

Predicate properties
--------------------

| predicate_property ::=
|    "``static``" \| "``dynamic``" \|
|    "``scope(``" scope "``)``" \|
|    "``private``" \| "``protected``" \| "``public``" \|
|    "``logtalk``" \| "``prolog``" \| "``foreign``" \|
|    "``coinductive(``" coinductive_predicate_template "``)``" \|
|    "``multifile``" \|
|    "``synchronized``" \|
|    "``built_in``" \|
|    "``inline``" \|
|    "``declared_in(``" entity_identifier "``)``" \|
|    "``defined_in(``" object_identifier \| category_identifier "``)``" \|
|    "``redefined_from(``" object_identifier \| category_identifier "``)``" \|
|    "``meta_predicate(``" meta_predicate_template "``)``" \|
|    "``alias_of(``" callable "``)``" \|
|    "``alias_declared_in(``" entity_identifier "``)``" \|
|    "``non_terminal(``" non_terminal_indicator "``)``" \|
|    "``mode(``" predicate_mode_term \| non_terminal_mode_term "``,``" number_of_proofs "``)``" \|
|    "``info(``" *list* "``)``" \|
|    "``number_of_clauses(``" *integer* "``)``" \|
|    "``number_of_rules(``" *integer* "``)``" \|
|    "``declared_in(``" entity_identifier "``,``" line_count "``)``" \|
|    "``defined_in(``" object_identifier \| category_identifier "``,``" line_count "``)``" \|
|    "``redefined_from(``" object_identifier \| category_identifier "``,``" line_count "``)``" \|
|    "``alias_declared_in(``" entity_identifier "``,``" line_count "``)``"

| line_count ::=
|    *integer*"

.. _grammar_compiler_flags:

Compiler flags
--------------

| compiler_flag ::=
|    *flag(flag_value)*
