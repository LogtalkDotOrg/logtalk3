<?php
/*************************************************************************************
 * logtalk.php
 * -----------
 *
 * Author: Paulo Moura (pmoura@logtalk.org)
 * Copyright: (c) 2009-2024 Paulo Moura (https://logtalk.org/)
 * Release Version: 1.0.8.6
 * Date Started: 2009/10/24
 *
 * Logtalk language file for GeSHi.
 *
 * CHANGES
 * -------
 * 2020/07/31 (1.1.27)
 *  -  Added syntax coloring of the consistency_error/3 error throwing built-in method
 * 2020/07/31 (1.1.26)
 *  -  Added syntax coloring of the uninstantiation_error/1 error throwing built-in method
 * 2019/07/08 (1.1.25)
 *  -  Fixed coloring of 0'Char numbers
 * 2019/02/12 (1.1.24)
 *  -  Added syntax coloring of the threaded_cancel/1 predicate
 * 2018/07/26 (1.1.23)
 *  -  Added syntax coloring of the logtalk_make_target_action/1 predicate
 * 2018/06/29 (1.1.22)
 *  -  Added syntax coloring of the error throwing built-in methods
 * 2017/11/13 (1.1.21)
 *  -  Added syntax coloring of the error throwing built-in methods
 * 2017/06/29 (1.1.20)
 *  -  Added syntax coloring of the context/1 method
 * 2016/06/18 (1.1.19)
 *  -  Fixed for the rename of some of the engine predicates
 * 2016/05/29 (1.1.18)
 *  -  Added syntax coloring for the engine predicates
 * 2015/04/24 (1.1.17)
 *  -  Added syntax coloring of the create_logtalk_flag/3 predicate
 * 2014/10/21 (1.1.16)
 *  -  Added syntax coloring of the atan2/2, div/2, tan/2, and xor/2 arithmetic functions
 * 2014/06/10 (1.1.15)
 *  -  Added syntax coloring of the as/2 alias operator
 * 2013/09/11 (1.1.14)
 *  -  Added syntax coloring of logtalk_load_context/2 predicate
 * 2013/09/11 (1.1.13)
 *  -  Added syntax coloring of logtalk_make/1 predicate
 * 2013/08/04 (1.1.12)
 *  -  Added syntax coloring of logtalk_make/0 predicate
 * 2013/05/04 (1.1.11)
 *  -  Added syntax coloring of built_in/0 directive
 * 2013/04/07 (1.1.10)
 *  -  Added syntax coloring of forward/1 method
 * 2013/02/06 (1.1.9)
 *  -  Added syntax coloring of false/0 predicate
 * 2012/05/03 (1.1.8)
 *  -  Added syntax coloring of acyclic_term/1 and subsumes_term/2 predicates
 * 2012/03/05 (1.1.7)
 *  -  Added syntax coloring of acos/1, asin/1, min/2, and max/2 functions
 *  -  Added syntax coloring of term_variables/2 predicate
 * 2011/07/24 (1.1.6)
 *  -  Added syntax coloring of include/1
 * 2011/01/18 (1.1.5)
 *  -  Added syntax coloring of meta_non_terminal/1
 * 2011/01/18 (1.1.4)
 *  -  Added syntax coloring of ignore/1
 * 2010/11/28 (1.1.3)
 *  -  Added syntax coloring of conforms_to_protocol/2-3
 * 2010/09/14 (1.1.2)
 *  -  Added syntax coloring of coinductive/1
 * 2010/06/23 (1.1.1)
 *  -  Added syntax coloring of e/0 and pi/0
 *  -  Added syntax coloring of ground/1, numbervars/3, keysort/2, and sort/2
 * 2010/05/15 (1.1.0)
 *  -  Added syntax coloring of callable/1 and compare/3
 * 2009/10/28 (1.0.0)
 *  -  First Release
 *
 *************************************************************************************
 *
 *     This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ************************************************************************************/

$language_data = array(
    'LANG_NAME' => 'Logtalk',
    'COMMENT_SINGLE' => array(1 => '%'),
    'COMMENT_MULTI' => array('/*' => '*/'),
    'COMMENT_REGEXP' => array(2 => "/0'[\\\\]?./sim"),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array("'"),
    'HARDQUOTE' => array('"', '"'),
    'HARDESCAPE' => array(),
    'ESCAPE_CHAR' => '',
    'ESCAPE_REGEXP' => array(
        //Simple Single Char Escapes
        1 => "#\\\\[\\\\abfnrtv\'\"?\n]#i",
        //Hexadecimal Char Specs
        2 => "#\\\\x[\da-fA-F]+\\\\#",
        //Octal Char Specs
        3 => "#\\\\[0-7]+\\\\#"
        ),
    'NUMBERS' =>
        GESHI_NUMBER_INT_BASIC |
        GESHI_NUMBER_BIN_PREFIX_0B |
        GESHI_NUMBER_OCT_PREFIX_0O |
        GESHI_NUMBER_HEX_PREFIX |
        GESHI_NUMBER_FLT_NONSCI |
        GESHI_NUMBER_FLT_SCI_ZERO,
    'KEYWORDS' => array(
        // Directives (with arguments)
        1 => array(
            // file directives
            'encoding', 'ensure_loaded', 'include',
            // flag directives
            'set_logtalk_flag', 'set_prolog_flag',
            // entity opening directives
            'category', 'object', 'protocol',
            // predicate scope directives
            'private', 'protected', 'public',
            // conditional compilation directives
            'elif', 'if',
            // entity directives
            'calls', 'initialization', 'op', 'uses',
            // predicate directives
            'alias', 'coinductive', 'discontiguous', 'dynamic', 'mode', 'info', 'meta_predicate', 'meta_non_terminal', 'multifile', 'synchronized',
            // module directives
            'export', 'module', 'reexport', 'use_module'
            ),
        // Directives (no arguments)
        2 => array(
            // entity directives
            'built_in', 'dynamic',
            // multi-threading directives
            'synchronized', 'threaded',
            // entity closing directives
            'end_category', 'end_object', 'end_protocol',
            // conditional compilation directives
            'else', 'endif'
            ),
        // Entity relations
        3 => array(
            'complements', 'extends', 'imports', 'implements','instantiates', 'specializes'
            ),
        // Built-in predicates (with arguments)
        4 => array(
            // event handlers
            'after', 'before', 'forward',
            // execution-context methods
            'context', 'parameter', 'self', 'sender', 'this',
            // predicate reflection
            'current_predicate', 'predicate_property',
            // DCGs and term expansion
            'expand_goal', 'expand_term', 'goal_expansion', 'phrase', 'term_expansion',
            // entity
            'abolish_category', 'abolish_object', 'abolish_protocol',
            'create_category', 'create_object', 'create_protocol',
            'current_category', 'current_object', 'current_protocol',
            'category_property', 'object_property', 'protocol_property',
            // entity relations
            'complements_object', 'conforms_to_protocol',
            'extends_category', 'extends_object', 'extends_protocol',
            'implements_protocol', 'imports_category',
            'instantiates_class', 'specializes_class',
            // events
            'abolish_events', 'current_event', 'define_events',
            // flags
            'current_logtalk_flag', 'set_logtalk_flag', 'create_logtalk_flag',
            'current_prolog_flag', 'set_prolog_flag',
            // compiling, loading, and library path
            'logtalk_compile', 'logtalk_library_path', 'logtalk_load', 'logtalk_load_context',
            'logtalk_make', 'logtalk_make_target_action',
            // database
            'abolish', 'asserta', 'assertz', 'clause', 'retract', 'retractall',
            // control
            'call', 'catch', 'ignore', 'once', 'throw',
            'uninstantiation_error', 'type_error', 'domain_error', 'consistency_error',
            'existence_error', 'permission_error', 'representation_error', 'evaluation_error',
            'resource_error', 'syntax_error',
            // all solutions predicates
            'bagof', 'findall', 'forall', 'setof',
            // multi-threading predicates
            'threaded',
            'threaded_call', 'threaded_once', 'threaded_ignore',
            'threaded_exit', 'threaded_peek', 'threaded_cancel',
            'threaded_wait', 'threaded_notify',
            // engine predicates
            'threaded_engine',
            'threaded_engine_create', 'threaded_engine_destroy', 'threaded_engine_self',
            'threaded_engine_next', 'threaded_engine_next_reified', 'threaded_engine_yield',
            'threaded_engine_post', 'threaded_engine_fetch',
            // term unification
            'subsumes_term', 'unify_with_occurs_check',
            // atomic term processing
            'atom_chars', 'atom_codes', 'atom_concat', 'atom_length',
            'number_chars', 'number_codes',
            'char_code',
            // term creation and decomposition
            'arg', 'copy_term', 'functor', 'numbervars', 'term_variables',
            // term testing
            'acyclic_term', 'atom', 'atomic', 'callable', 'compound', 'float', 'ground', 'integer', 'nonvar', 'number', 'sub_atom', 'var',
            // term comparison
            'compare',
            // stream selection and control
            'current_input', 'current_output', 'set_input', 'set_output',
            'open', 'close', 'flush_output', 'stream_property',
            'at_end_of_stream', 'set_stream_position',
            // character and byte input/output predicates
            'get_byte', 'get_char', 'get_code',
            'peek_byte', 'peek_char', 'peek_code',
            'put_byte', 'put_char', 'put_code',
            'nl',
            // term input/output predicates
            'current_op', 'op',
            'write', 'writeq', 'write_canonical', 'write_term',
            'read', 'read_term',
            'char_conversion', 'current_char_conversion',
            // hooks
            'halt',
            // sorting
            'keysort', 'sort'
            ),
        // Built-in predicates (no arguments)
        5 => array(
            // control
            'fail', 'false', 'repeat', 'true', 'instantiation_error', 'system_error',
            // character and byte input/output predicates
            'nl',
            // implementation defined hooks functions
            'halt',
            // arithemtic evaluation
            'is',
            // stream selection and control
            'at_end_of_stream', 'flush_output'
            ),
        // Evaluable functors (with arguments)
        6 => array(
            'float_integer_part', 'float_fractional_part',
            'div', 'rem', 'mod', 'abs', 'sign', 'floor', 'truncate', 'round', 'ceiling',
            'cos', 'acos', 'asin', 'atan', 'atan2', 'exp', 'log', 'sin', 'sqrt', 'tan',
            'min', 'max',
            'xor'
            ),
        // Evaluable functors (no arguments)
        7 => array(
            'e', 'pi', 'div', 'mod', 'rem'
            ),
        ),
    'SYMBOLS' => array(
        0 => array(
            // external call
            '{', '}'
            ),
        1 => array(
            // arithemtic comparison
            '=:=', '=\=', '<', '=<', '>=', '>',
            // term comparison
            '<<', '>>', '/\\', '\\/', '\\',
            // bitwise functors
            '==', '\==', '@<', '@=<', '@>=', '@>',
            // evaluable functors
            '+', '-', '*', '/', '**',
            // logic and control
            '!', '\\+', ';',
            // message sending operators
            '::', '^^', ':',
            // grammar rule and conditional functors
            '-->', '->',
            // mode operators
            '@', '?',
            // term to list predicate
            '=..',
            // unification
            '=', '\\=',
            // alias operator
            'as'
            ),
        2 => array(
            // clause and directive functors
            ':-'
            )
        ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true,
        2 => true,
        3 => true,
        4 => true,
        5 => true,
        6 => true,
        7 => true
        ),
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => 'color: #2e4dc9;',
            2 => 'color: #2e4dc9;',
            3 => 'color: #2e4dc9;',
            4 => 'color: #9d4f37;',
            5 => 'color: #9d4f37;',
            6 => 'color: #9d4f37;',
            7 => 'color: #9d4f37;'
            ),
        'NUMBERS' => array(
            0 => 'color: #430000;'
            ),
        'COMMENTS' => array(
            1 => 'color: #60a0b0; font-style: italic;',
            2 => 'color: #430000;',
            'MULTI' => 'color: #60a0b0; font-style: italic;'
            ),
        'ESCAPE_CHAR' => array(
            0 => 'color: #9f0000; font-weight: bold;',
            1 => 'color: #9f0000; font-weight: bold;',
            2 => 'color: #9f0000; font-weight: bold;',
            3 => 'color: #9f0000; font-weight: bold;',
            'HARD' => '',
            ),
        'SYMBOLS' => array(
            0 => 'color: #666666;font-weight: bold;',
            1 => 'color: #666666;font-weight: bold;',
            2 => 'color: #000000;'
            ),
        'BRACKETS' => array(
            0 => 'color: #000000;'
            ),
        'STRINGS' => array(
            0 => 'color: #9f0000;',
            'HARD' => 'color: #9f0000;'
            ),
        'METHODS' => array(
            ),
        'REGEXPS' => array(
            0 => 'color: #848484;'
            ),
        'SCRIPT' => array()
        ),
    'URLS' => array(
        1 => '',
        2 => '',
        3 => '',
        4 => '',
        5 => '',
        6 => '',
        7 => ''
        ),
    'OOLANG' => false,
    'OBJECT_SPLITTERS' => array(
        1 => '::'
        ),
    'REGEXPS' => array(
        // variables
        0 => '\b(?!(?:PIPE|SEMI|REG3XP\d*)[^a-zA-Z0-9_])[A-Z_][a-zA-Z0-9_]*(?![a-zA-Z0-9_])'
        ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(),
    'HIGHLIGHT_STRICT_BLOCK' => array(),
    'TAB_WIDTH' => 4,
    'PARSER_CONTROL' => array(
        'ENABLE_FLAGS' => array(
            'BRACKETS' => GESHI_NEVER
        ),
        'KEYWORDS' => array(
            1 => array(
                'DISALLOWED_BEFORE' => '(?<=:-\s)',
                'DISALLOWED_AFTER' => '(?=\()'
            ),
            2 => array(
                'DISALLOWED_BEFORE' => '(?<=:-\s)',
                'DISALLOWED_AFTER' => '(?=\.)'
            ),
            3 => array(
                'DISALLOWED_BEFORE' => '\b',
                'DISALLOWED_AFTER' => '(?=\()'
            ),
            4 => array(
                'DISALLOWED_BEFORE' => '\b',
                'DISALLOWED_AFTER' => '(?=\()'
            ),
            5 => array(
                'DISALLOWED_BEFORE' => '\b',
                'DISALLOWED_AFTER' => '\b'
            ),
            6 => array(
                'DISALLOWED_BEFORE' => '\b',
                'DISALLOWED_AFTER' => '(?=\()'
            ),
            7 => array(
                'DISALLOWED_BEFORE' => '\b',
                'DISALLOWED_AFTER' => '\b'
            )
        )
    ),
);

?>
