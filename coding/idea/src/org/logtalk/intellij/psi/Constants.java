package org.logtalk.intellij.psi;

import static java.util.Arrays.asList;

import java.util.HashSet;
import java.util.Set;

/**
 * Implementation note:
 * This should be a temporary solution only.
 * Constants hardcoded in this class should rather be dynamic, obtained from a proper Prolog query.
 */
public class Constants {

    public static final Set<String> ATOM_KEYWORDS = new HashSet<>(asList( //newHashSet(
            "else" , "endif" , "dynamic" ,
            "end_category" , "end_object" , "end_protocol" , "threaded" , "eos" , "logtalk_make" , "built_in" , "halt" ,
            "flush_output" , "at_end_of_stream" , "true" , "fail" , "false" , "repeat" , "nl" , "pi" , "e"));

    public static final Set<String> COMPOUND_NAME_KEYWORDS = new HashSet<>(asList( //newHashSet
            "encoding" , "initialization" , "op" ,
            "set_logtalk_flag" , "if" , "elif" , "calls" , "category" ,
            "include" , "info" , "object" , "protocol" ,
            "uses" , "alias" , "coinductive" , "discontiguous" , "dynamic" , "meta_predicate" , "meta_non_terminal" , "mode" ,
            "multifile" , "private" , "protected" , "public" , "synchronized" , "use_module" ,
            "context", "parameter" , "self" , "sender" , "this" , "current_op" , "current_predicate" , "predicate_property" , "abolish" , "asserta" ,
            "assertz" , "clause" , "retract" , "retractall" , "call" , "once" , "catch" , "throw" ,
			"instantiation_error" , "type_error" , "domain_error" , "existence_error" ,
			"permission_error" , "representation_error" , "evaluation_error", "resource_error" ,
			"bagof" , "findall" , "forall" , "setof" ,
			"before" , "after" , "forward" , "phrase" , "expand_term" , "term_expansion" , "expand_goal" ,
            "goal_expansion" , "coinductive_success_hook" , "ask_question" , "message_hook" ,
            "message_prefix_stream" , "print_message" , "print_message_tokens" , "print_message_token" , "question_hook" , "question_prompt_stream",
            "call" , "message_tokens" ,
            "current_category" , "current_object" , "current_protocol" , "category_property" , "object_property" , "protocol_property" ,
            "create_category" , "create_object" , "create_protocol" , "abolish_category" , "abolish_object" , "abolish_protocol" ,
            "extends_object" , "extends_protocol" , "extends_category" ,
            "implements_protocol" , "imports_category" , "instantiates_class" ,
            "specializes_class" , "complements_object" , "abolish_events" , "current_event" , "define_events" ,
            "threaded" , "threaded_call" , "threaded_once" , "threaded_ignore" , "threaded_exit" ,
            "threaded_peek" , "threaded_wait" , "threaded_notify" , "threaded_engine" , "threaded_engine_create" , "threaded_engine_destroy" ,
            "threaded_engine_self" , "threaded_engine_next" , "threaded_engine_next_reified" , "threaded_engine_yield" , "threaded_engine_post" ,
            "threaded_engine_fetch" , "logtalk_compile" , "logtalk_load" , "logtalk_make" , "logtalk_library_path" ,
            "logtalk_load_context" , "current_logtalk_flag" , "create_logtalk_flag" ,
            "implements" , "imports" , "complements" , "extends" , "instantiates" , "specializes" ,
            "ensure_loaded" , "export" , "reexport" , "module" , "set_prolog_flag" ,
            "unify_with_occurs_check" , "subsumes_term" , "atom" , "atomic" , "integer" , "float" , "callable" , "compound" , "nonvar" ,
            "var" , "number" , "ground" , "acyclic_term" , "compare" , "functor" , "arg" , "copy_term" , "numbervars" , "term_variables" ,
            "current_input" , "current_output" , "set_input" , "set_output" , "open" , "close" , "flush_output" , "stream_property" ,
            "at_end_of_stream" , "set_stream_position" , "get_char" , "get_code" , "peek_char" , "peek_code" , "put_char" , "put_code" ,
            "nl" , "get_byte" , "peek_byte" , "put_byte" , "read" , "read_term" , "writeq" , "write" , "write_canonical" , "write_term" ,
            "current_char_conversion" , "char_conversion" , "ignore" , "atom_length" , "atom_chars" ,
            "atom_codes" , "atom_concat" , "sub_atom" , "char_code" , "number_chars" , "number_codes" , "current_prolog_flag" ,
            "halt" , "keysort" , "sort" ,
            "atan" , "atan2" , "acos" , "asin" , "sin" , "cos" , "tan" , "sign" , "abs" , "truncate" , "round" , "ceiling" , "exp" ,
            "log" , "sqrt" , "rem" , "mod" , "div" , "float_fractional_part" , "float_integer_part" , "floor" , "min" , "max" , "xor"));


}
