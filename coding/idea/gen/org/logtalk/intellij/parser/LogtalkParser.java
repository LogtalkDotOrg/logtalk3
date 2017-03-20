// This is a generated file. Not intended for manual editing.
package org.logtalk.intellij.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static org.logtalk.intellij.psi.LogtalkTypes.*;
import static com.intellij.lang.parser.GeneratedParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class LogtalkParser implements PsiParser, LightPsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, null);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    if (t == ATOM) {
      r = atom(b, 0);
    }
    else if (t == ATOM_KEYWORD) {
      r = atom_keyword(b, 0);
    }
    else if (t == BASIC_TERM) {
      r = basic_term(b, 0);
    }
    else if (t == BRACED_BLOCK) {
      r = braced_block(b, 0);
    }
    else if (t == COMPOUND) {
      r = compound(b, 0);
    }
    else if (t == COMPOUND_NAME) {
      r = compound_name(b, 0);
    }
    else if (t == COMPOUND_NAME_KEYWORD) {
      r = compound_name_keyword(b, 0);
    }
    else if (t == CUSTOM_BINARY_OPERATION) {
      r = custom_binary_operation(b, 0);
    }
    else if (t == CUSTOM_LEFT_OPERATION) {
      r = custom_left_operation(b, 0);
    }
    else if (t == KNOWN_BINARY_OPERATOR) {
      r = known_binary_operator(b, 0);
    }
    else if (t == KNOWN_LEFT_OPERATOR) {
      r = known_left_operator(b, 0);
    }
    else if (t == LIST) {
      r = list(b, 0);
    }
    else if (t == MAP_REFERENCE) {
      r = map_reference(b, 0);
    }
    else if (t == NATIVE_BINARY_OPERATION) {
      r = native_binary_operation(b, 0);
    }
    else if (t == NATIVE_LEFT_OPERATION) {
      r = native_left_operation(b, 0);
    }
    else if (t == NUMBER) {
      r = number(b, 0);
    }
    else if (t == OPERATION) {
      r = operation(b, 0);
    }
    else if (t == PARENTHESIZED_BLOCK) {
      r = parenthesized_block(b, 0);
    }
    else if (t == SENTENCE) {
      r = sentence(b, 0);
    }
    else if (t == TERM) {
      r = term(b, 0);
    }
    else if (t == VALID_OPERATOR) {
      r = valid_operator(b, 0);
    }
    else if (t == VARIABLE) {
      r = variable(b, 0);
    }
    else {
      r = parse_root_(t, b, 0);
    }
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return logtalkFile(b, l + 1);
  }

  /* ********************************************************** */
  // atom_keyword|
  //             UNQUOTED_ATOM|
  //             SYMBOLIC_ATOM|
  //             CUT|
  //             QUOTED_ATOM
  public static boolean atom(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ATOM, "<atom>");
    r = atom_keyword(b, l + 1);
    if (!r) r = consumeToken(b, UNQUOTED_ATOM);
    if (!r) r = consumeToken(b, SYMBOLIC_ATOM);
    if (!r) r = consumeToken(b, CUT);
    if (!r) r = consumeToken(b, QUOTED_ATOM);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'else' | 'endif' | 'dynamic' | 'end_category' | 'end_object' | 'end_protocol' | 'threaded' |
  // 'eos' |
  // 'logtalk_make' |
  // 'built_in' |
  // 'halt' | 'flush_output' | 'at_end_of_stream' | 'true' | 'fail' | 'false' | 'repeat' | 'nl' |
  // 'pi' | 'e'
  public static boolean atom_keyword(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_keyword")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ATOM_KEYWORD, "<atom keyword>");
    r = consumeToken(b, "else");
    if (!r) r = consumeToken(b, "endif");
    if (!r) r = consumeToken(b, "dynamic");
    if (!r) r = consumeToken(b, "end_category");
    if (!r) r = consumeToken(b, "end_object");
    if (!r) r = consumeToken(b, "end_protocol");
    if (!r) r = consumeToken(b, "threaded");
    if (!r) r = consumeToken(b, "eos");
    if (!r) r = consumeToken(b, "logtalk_make");
    if (!r) r = consumeToken(b, "built_in");
    if (!r) r = consumeToken(b, "halt");
    if (!r) r = consumeToken(b, "flush_output");
    if (!r) r = consumeToken(b, "at_end_of_stream");
    if (!r) r = consumeToken(b, "true");
    if (!r) r = consumeToken(b, "fail");
    if (!r) r = consumeToken(b, "false");
    if (!r) r = consumeToken(b, "repeat");
    if (!r) r = consumeToken(b, "nl");
    if (!r) r = consumeToken(b, "pi");
    if (!r) r = consumeToken(b, "e");
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // parenthesized_block|braced_block|list|map_reference|number|variable|STRING|compound|atom
  public static boolean basic_term(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "basic_term")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, BASIC_TERM, "<basic term>");
    r = parenthesized_block(b, l + 1);
    if (!r) r = braced_block(b, l + 1);
    if (!r) r = list(b, l + 1);
    if (!r) r = map_reference(b, l + 1);
    if (!r) r = number(b, l + 1);
    if (!r) r = variable(b, l + 1);
    if (!r) r = consumeToken(b, STRING);
    if (!r) r = compound(b, l + 1);
    if (!r) r = atom(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // LBRACE term RBRACE
  public static boolean braced_block(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "braced_block")) return false;
    if (!nextTokenIs(b, LBRACE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACE);
    r = r && term(b, l + 1);
    r = r && consumeToken(b, RBRACE);
    exit_section_(b, m, BRACED_BLOCK, r);
    return r;
  }

  /* ********************************************************** */
  // compound_name LPAREN term RPAREN
  public static boolean compound(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compound")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, COMPOUND, "<compound>");
    r = compound_name(b, l + 1);
    r = r && consumeToken(b, LPAREN);
    r = r && term(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // compound_name_keyword|
  //                     UNQUOTED_COMPOUND_NAME|
  //                     QUOTED_COMPOUND_NAME
  public static boolean compound_name(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compound_name")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, COMPOUND_NAME, "<compound name>");
    r = compound_name_keyword(b, l + 1);
    if (!r) r = consumeToken(b, UNQUOTED_COMPOUND_NAME);
    if (!r) r = consumeToken(b, QUOTED_COMPOUND_NAME);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'encoding' | 'initialization' | 'op' | 'set_logtalk_flag' | 'if' | 'elif' | 'calls' | 'category' |
  // 'include' | 'info' | 'object' | 'protocol' |
  // 'uses' | 'alias' | 'coinductive' | 'discontiguous' | 'dynamic' | 'meta_predicate' | 'meta_non_terminal' | 'mode' |
  // 'multifile' | 'private' | 'protected' | 'public' | 'synchronized' | 'use_module' |
  // 'parameter' | 'self' | 'sender' | 'this' | 'current_op' | 'current_predicate' | 'predicate_property' | 'abolish' | 'asserta' |
  //  'assertz' | 'clause' | 'retract' | 'retractall' | 'call' | 'once' | 'catch' | 'throw' | 'bagof' | 'findall' | 'forall' |
  //  'setof' | 'before' | 'after' | 'forward' | 'phrase' | 'expand_term' | 'term_expansion' | 'expand_goal' |
  //  'goal_expansion' | 'coinductive_success_hook' | 'ask_question' | 'message_hook' |
  //  'message_prefix_stream' | 'print_message' | 'print_message_tokens' | 'print_message_token' | 'question_hook' | 'question_prompt_stream'|
  //  'call' 'message_tokens'|
  //  'current_category' | 'current_object' | 'current_protocol' | 'category_property' | 'object_property' | 'protocol_property' |
  //  'create_category' | 'create_object' | 'create_protocol' | 'abolish_category' | 'abolish_object' | 'abolish_protocol' |
  //  'extends_object' | 'extends_protocol' | 'extends_category' |
  //  'implements_protocol' | 'imports_category' | 'instantiates_class' |
  //  'specializes_class' | 'complements_object' | 'abolish_events' | 'current_event' | 'define_events' |
  //  'threaded' | 'threaded_call' | 'threaded_once' | 'threaded_ignore' | 'threaded_exit' |
  //  'threaded_peek' | 'threaded_wait' | 'threaded_notify' | 'threaded_engine' | 'threaded_engine_create' | 'threaded_engine_destroy' |
  //  'threaded_engine_self' | 'threaded_engine_next' | 'threaded_engine_next_reified' | 'threaded_engine_yield' | 'threaded_engine_post' |
  //  'threaded_engine_fetch' | 'logtalk_compile' | 'logtalk_load' | 'logtalk_make' | 'logtalk_library_path' |
  //  'logtalk_load_context' | 'current_logtalk_flag' | 'create_logtalk_flag' |
  //  'implements' | 'imports' | 'complements' | 'extends' | 'instantiates' | 'specializes' |
  //  'ensure_loaded' | 'export' | 'reexport' | 'module' | 'set_prolog_flag' |
  //  'unify_with_occurs_check' | 'subsumes_term' | 'atom' | 'atomic' | 'integer' | 'float' | 'callable' | 'compound' | 'nonvar' |
  //  'var' | 'number' | 'ground' | 'acyclic_term' | 'compare' | 'functor' | 'arg' | 'copy_term' | 'numbervars' | 'term_variables' |
  //  'current_input' | 'current_output' | 'set_input' | 'set_output' | 'open' | 'close' | 'flush_output' | 'stream_property' |
  //  'at_end_of_stream' | 'set_stream_position' | 'get_char' | 'get_code' | 'peek_char' | 'peek_code' | 'put_char' | 'put_code' |
  //  'nl' | 'get_byte' | 'peek_byte' | 'put_byte' | 'read' | 'read_term' | 'writeq' | 'write' | 'write_canonical' | 'write_term' |
  //  'current_char_conversion' | 'char_conversion' | 'ignore' | 'atom_length' | 'atom_chars' |
  //  'atom_codes' | 'atom_concat' | 'sub_atom' | 'char_code' | 'number_chars' | 'number_codes' | 'current_prolog_flag' |
  //  'halt' | 'keysort' | 'sort' |
  //  'atan' | 'atan2' | 'acos' | 'asin' | 'sin' | 'cos' | 'tan' | 'sign' | 'abs' | 'truncate' | 'round' | 'ceiling' | 'exp' |
  //  'log' | 'sqrt' | 'rem' | 'mod' | 'div' | 'float_fractional_part' | 'float_integer_part' | 'floor' | 'min' | 'max' | 'xor'
  public static boolean compound_name_keyword(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compound_name_keyword")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, COMPOUND_NAME_KEYWORD, "<compound name keyword>");
    r = consumeToken(b, "encoding");
    if (!r) r = consumeToken(b, "initialization");
    if (!r) r = consumeToken(b, "op");
    if (!r) r = consumeToken(b, "set_logtalk_flag");
    if (!r) r = consumeToken(b, "if");
    if (!r) r = consumeToken(b, "elif");
    if (!r) r = consumeToken(b, "calls");
    if (!r) r = consumeToken(b, "category");
    if (!r) r = consumeToken(b, "include");
    if (!r) r = consumeToken(b, "info");
    if (!r) r = consumeToken(b, "object");
    if (!r) r = consumeToken(b, "protocol");
    if (!r) r = consumeToken(b, "uses");
    if (!r) r = consumeToken(b, "alias");
    if (!r) r = consumeToken(b, "coinductive");
    if (!r) r = consumeToken(b, "discontiguous");
    if (!r) r = consumeToken(b, "dynamic");
    if (!r) r = consumeToken(b, "meta_predicate");
    if (!r) r = consumeToken(b, "meta_non_terminal");
    if (!r) r = consumeToken(b, "mode");
    if (!r) r = consumeToken(b, "multifile");
    if (!r) r = consumeToken(b, "private");
    if (!r) r = consumeToken(b, "protected");
    if (!r) r = consumeToken(b, "public");
    if (!r) r = consumeToken(b, "synchronized");
    if (!r) r = consumeToken(b, "use_module");
    if (!r) r = consumeToken(b, "parameter");
    if (!r) r = consumeToken(b, "self");
    if (!r) r = consumeToken(b, "sender");
    if (!r) r = consumeToken(b, "this");
    if (!r) r = consumeToken(b, "current_op");
    if (!r) r = consumeToken(b, "current_predicate");
    if (!r) r = consumeToken(b, "predicate_property");
    if (!r) r = consumeToken(b, "abolish");
    if (!r) r = consumeToken(b, "asserta");
    if (!r) r = consumeToken(b, "assertz");
    if (!r) r = consumeToken(b, "clause");
    if (!r) r = consumeToken(b, "retract");
    if (!r) r = consumeToken(b, "retractall");
    if (!r) r = consumeToken(b, "call");
    if (!r) r = consumeToken(b, "once");
    if (!r) r = consumeToken(b, "catch");
    if (!r) r = consumeToken(b, "throw");
    if (!r) r = consumeToken(b, "bagof");
    if (!r) r = consumeToken(b, "findall");
    if (!r) r = consumeToken(b, "forall");
    if (!r) r = consumeToken(b, "setof");
    if (!r) r = consumeToken(b, "before");
    if (!r) r = consumeToken(b, "after");
    if (!r) r = consumeToken(b, "forward");
    if (!r) r = consumeToken(b, "phrase");
    if (!r) r = consumeToken(b, "expand_term");
    if (!r) r = consumeToken(b, "term_expansion");
    if (!r) r = consumeToken(b, "expand_goal");
    if (!r) r = consumeToken(b, "goal_expansion");
    if (!r) r = consumeToken(b, "coinductive_success_hook");
    if (!r) r = consumeToken(b, "ask_question");
    if (!r) r = consumeToken(b, "message_hook");
    if (!r) r = consumeToken(b, "message_prefix_stream");
    if (!r) r = consumeToken(b, "print_message");
    if (!r) r = consumeToken(b, "print_message_tokens");
    if (!r) r = consumeToken(b, "print_message_token");
    if (!r) r = consumeToken(b, "question_hook");
    if (!r) r = consumeToken(b, "question_prompt_stream");
    if (!r) r = compound_name_keyword_64(b, l + 1);
    if (!r) r = consumeToken(b, "current_category");
    if (!r) r = consumeToken(b, "current_object");
    if (!r) r = consumeToken(b, "current_protocol");
    if (!r) r = consumeToken(b, "category_property");
    if (!r) r = consumeToken(b, "object_property");
    if (!r) r = consumeToken(b, "protocol_property");
    if (!r) r = consumeToken(b, "create_category");
    if (!r) r = consumeToken(b, "create_object");
    if (!r) r = consumeToken(b, "create_protocol");
    if (!r) r = consumeToken(b, "abolish_category");
    if (!r) r = consumeToken(b, "abolish_object");
    if (!r) r = consumeToken(b, "abolish_protocol");
    if (!r) r = consumeToken(b, "extends_object");
    if (!r) r = consumeToken(b, "extends_protocol");
    if (!r) r = consumeToken(b, "extends_category");
    if (!r) r = consumeToken(b, "implements_protocol");
    if (!r) r = consumeToken(b, "imports_category");
    if (!r) r = consumeToken(b, "instantiates_class");
    if (!r) r = consumeToken(b, "specializes_class");
    if (!r) r = consumeToken(b, "complements_object");
    if (!r) r = consumeToken(b, "abolish_events");
    if (!r) r = consumeToken(b, "current_event");
    if (!r) r = consumeToken(b, "define_events");
    if (!r) r = consumeToken(b, "threaded");
    if (!r) r = consumeToken(b, "threaded_call");
    if (!r) r = consumeToken(b, "threaded_once");
    if (!r) r = consumeToken(b, "threaded_ignore");
    if (!r) r = consumeToken(b, "threaded_exit");
    if (!r) r = consumeToken(b, "threaded_peek");
    if (!r) r = consumeToken(b, "threaded_wait");
    if (!r) r = consumeToken(b, "threaded_notify");
    if (!r) r = consumeToken(b, "threaded_engine");
    if (!r) r = consumeToken(b, "threaded_engine_create");
    if (!r) r = consumeToken(b, "threaded_engine_destroy");
    if (!r) r = consumeToken(b, "threaded_engine_self");
    if (!r) r = consumeToken(b, "threaded_engine_next");
    if (!r) r = consumeToken(b, "threaded_engine_next_reified");
    if (!r) r = consumeToken(b, "threaded_engine_yield");
    if (!r) r = consumeToken(b, "threaded_engine_post");
    if (!r) r = consumeToken(b, "threaded_engine_fetch");
    if (!r) r = consumeToken(b, "logtalk_compile");
    if (!r) r = consumeToken(b, "logtalk_load");
    if (!r) r = consumeToken(b, "logtalk_make");
    if (!r) r = consumeToken(b, "logtalk_library_path");
    if (!r) r = consumeToken(b, "logtalk_load_context");
    if (!r) r = consumeToken(b, "current_logtalk_flag");
    if (!r) r = consumeToken(b, "create_logtalk_flag");
    if (!r) r = consumeToken(b, "implements");
    if (!r) r = consumeToken(b, "imports");
    if (!r) r = consumeToken(b, "complements");
    if (!r) r = consumeToken(b, "extends");
    if (!r) r = consumeToken(b, "instantiates");
    if (!r) r = consumeToken(b, "specializes");
    if (!r) r = consumeToken(b, "ensure_loaded");
    if (!r) r = consumeToken(b, "export");
    if (!r) r = consumeToken(b, "reexport");
    if (!r) r = consumeToken(b, "module");
    if (!r) r = consumeToken(b, "set_prolog_flag");
    if (!r) r = consumeToken(b, "unify_with_occurs_check");
    if (!r) r = consumeToken(b, "subsumes_term");
    if (!r) r = consumeToken(b, "atom");
    if (!r) r = consumeToken(b, "atomic");
    if (!r) r = consumeToken(b, "integer");
    if (!r) r = consumeToken(b, "float");
    if (!r) r = consumeToken(b, "callable");
    if (!r) r = consumeToken(b, "compound");
    if (!r) r = consumeToken(b, "nonvar");
    if (!r) r = consumeToken(b, "var");
    if (!r) r = consumeToken(b, "number");
    if (!r) r = consumeToken(b, "ground");
    if (!r) r = consumeToken(b, "acyclic_term");
    if (!r) r = consumeToken(b, "compare");
    if (!r) r = consumeToken(b, "functor");
    if (!r) r = consumeToken(b, "arg");
    if (!r) r = consumeToken(b, "copy_term");
    if (!r) r = consumeToken(b, "numbervars");
    if (!r) r = consumeToken(b, "term_variables");
    if (!r) r = consumeToken(b, "current_input");
    if (!r) r = consumeToken(b, "current_output");
    if (!r) r = consumeToken(b, "set_input");
    if (!r) r = consumeToken(b, "set_output");
    if (!r) r = consumeToken(b, "open");
    if (!r) r = consumeToken(b, "close");
    if (!r) r = consumeToken(b, "flush_output");
    if (!r) r = consumeToken(b, "stream_property");
    if (!r) r = consumeToken(b, "at_end_of_stream");
    if (!r) r = consumeToken(b, "set_stream_position");
    if (!r) r = consumeToken(b, "get_char");
    if (!r) r = consumeToken(b, "get_code");
    if (!r) r = consumeToken(b, "peek_char");
    if (!r) r = consumeToken(b, "peek_code");
    if (!r) r = consumeToken(b, "put_char");
    if (!r) r = consumeToken(b, "put_code");
    if (!r) r = consumeToken(b, "nl");
    if (!r) r = consumeToken(b, "get_byte");
    if (!r) r = consumeToken(b, "peek_byte");
    if (!r) r = consumeToken(b, "put_byte");
    if (!r) r = consumeToken(b, "read");
    if (!r) r = consumeToken(b, "read_term");
    if (!r) r = consumeToken(b, "writeq");
    if (!r) r = consumeToken(b, "write");
    if (!r) r = consumeToken(b, "write_canonical");
    if (!r) r = consumeToken(b, "write_term");
    if (!r) r = consumeToken(b, "current_char_conversion");
    if (!r) r = consumeToken(b, "char_conversion");
    if (!r) r = consumeToken(b, "ignore");
    if (!r) r = consumeToken(b, "atom_length");
    if (!r) r = consumeToken(b, "atom_chars");
    if (!r) r = consumeToken(b, "atom_codes");
    if (!r) r = consumeToken(b, "atom_concat");
    if (!r) r = consumeToken(b, "sub_atom");
    if (!r) r = consumeToken(b, "char_code");
    if (!r) r = consumeToken(b, "number_chars");
    if (!r) r = consumeToken(b, "number_codes");
    if (!r) r = consumeToken(b, "current_prolog_flag");
    if (!r) r = consumeToken(b, "halt");
    if (!r) r = consumeToken(b, "keysort");
    if (!r) r = consumeToken(b, "sort");
    if (!r) r = consumeToken(b, "atan");
    if (!r) r = consumeToken(b, "atan2");
    if (!r) r = consumeToken(b, "acos");
    if (!r) r = consumeToken(b, "asin");
    if (!r) r = consumeToken(b, "sin");
    if (!r) r = consumeToken(b, "cos");
    if (!r) r = consumeToken(b, "tan");
    if (!r) r = consumeToken(b, "sign");
    if (!r) r = consumeToken(b, "abs");
    if (!r) r = consumeToken(b, "truncate");
    if (!r) r = consumeToken(b, "round");
    if (!r) r = consumeToken(b, "ceiling");
    if (!r) r = consumeToken(b, "exp");
    if (!r) r = consumeToken(b, "log");
    if (!r) r = consumeToken(b, "sqrt");
    if (!r) r = consumeToken(b, "rem");
    if (!r) r = consumeToken(b, "mod");
    if (!r) r = consumeToken(b, "div");
    if (!r) r = consumeToken(b, "float_fractional_part");
    if (!r) r = consumeToken(b, "float_integer_part");
    if (!r) r = consumeToken(b, "floor");
    if (!r) r = consumeToken(b, "min");
    if (!r) r = consumeToken(b, "max");
    if (!r) r = consumeToken(b, "xor");
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // 'call' 'message_tokens'
  private static boolean compound_name_keyword_64(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compound_name_keyword_64")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "call");
    r = r && consumeToken(b, "message_tokens");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // basic_term atom term
  public static boolean custom_binary_operation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "custom_binary_operation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, CUSTOM_BINARY_OPERATION, "<custom binary operation>");
    r = basic_term(b, l + 1);
    r = r && atom(b, l + 1);
    r = r && term(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // atom term
  public static boolean custom_left_operation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "custom_left_operation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, CUSTOM_LEFT_OPERATION, "<custom left operation>");
    r = atom(b, l + 1);
    r = r && term(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // sentence|COMMENT|CRLF
  static boolean item_(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "item_")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = sentence(b, l + 1);
    if (!r) r = consumeToken(b, COMMENT);
    if (!r) r = consumeToken(b, CRLF);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // &valid_operator (
  //                             '-->'|
  //                             ':-'|
  //                             ';'|'|'|
  //                             '->'|'*->'|
  //                             ','|
  //                             ':='|
  //                             '<'|'='|'=..'|'=@='|'\=@='|
  //                             '=:='|'=<'|'=='|'=\='|
  //                             '>'|'>='|'@<'|'@=<'|
  //                             '@>'|'@>='|'\='|'\=='|
  //                             'as'|'is'|'>:<'|':<'|
  //                             ':'|
  //                             '+'|'-'|'/\'|'\/'|'xor'|
  //                             '*'|'/'|'//'|'div'|'rdiv'|'>>'|'mod'|'rem'|
  //                             '**'|
  //                             '^'|
  //                             '::'|
  //                             '^^'|
  //                             '<<'
  //                             )
  public static boolean known_binary_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_binary_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, KNOWN_BINARY_OPERATOR, "<known binary operator>");
    r = known_binary_operator_0(b, l + 1);
    r = r && known_binary_operator_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // &valid_operator
  private static boolean known_binary_operator_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_binary_operator_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_);
    r = valid_operator(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // '-->'|
  //                             ':-'|
  //                             ';'|'|'|
  //                             '->'|'*->'|
  //                             ','|
  //                             ':='|
  //                             '<'|'='|'=..'|'=@='|'\=@='|
  //                             '=:='|'=<'|'=='|'=\='|
  //                             '>'|'>='|'@<'|'@=<'|
  //                             '@>'|'@>='|'\='|'\=='|
  //                             'as'|'is'|'>:<'|':<'|
  //                             ':'|
  //                             '+'|'-'|'/\'|'\/'|'xor'|
  //                             '*'|'/'|'//'|'div'|'rdiv'|'>>'|'mod'|'rem'|
  //                             '**'|
  //                             '^'|
  //                             '::'|
  //                             '^^'|
  //                             '<<'
  private static boolean known_binary_operator_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_binary_operator_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "-->");
    if (!r) r = consumeToken(b, ":-");
    if (!r) r = consumeToken(b, ";");
    if (!r) r = consumeToken(b, "|");
    if (!r) r = consumeToken(b, "->");
    if (!r) r = consumeToken(b, "*->");
    if (!r) r = consumeToken(b, ",");
    if (!r) r = consumeToken(b, ":=");
    if (!r) r = consumeToken(b, "<");
    if (!r) r = consumeToken(b, "=");
    if (!r) r = consumeToken(b, "=..");
    if (!r) r = consumeToken(b, "=@=");
    if (!r) r = consumeToken(b, "\\=@=");
    if (!r) r = consumeToken(b, "=:=");
    if (!r) r = consumeToken(b, "=<");
    if (!r) r = consumeToken(b, "==");
    if (!r) r = consumeToken(b, "=\\=");
    if (!r) r = consumeToken(b, ">");
    if (!r) r = consumeToken(b, ">=");
    if (!r) r = consumeToken(b, "@<");
    if (!r) r = consumeToken(b, "@=<");
    if (!r) r = consumeToken(b, "@>");
    if (!r) r = consumeToken(b, "@>=");
    if (!r) r = consumeToken(b, "\\=");
    if (!r) r = consumeToken(b, "\\==");
    if (!r) r = consumeToken(b, "as");
    if (!r) r = consumeToken(b, "is");
    if (!r) r = consumeToken(b, ">:<");
    if (!r) r = consumeToken(b, ":<");
    if (!r) r = consumeToken(b, ":");
    if (!r) r = consumeToken(b, "+");
    if (!r) r = consumeToken(b, "-");
    if (!r) r = consumeToken(b, "/\\");
    if (!r) r = consumeToken(b, "\\/");
    if (!r) r = consumeToken(b, "xor");
    if (!r) r = consumeToken(b, "*");
    if (!r) r = consumeToken(b, "/");
    if (!r) r = consumeToken(b, "//");
    if (!r) r = consumeToken(b, "div");
    if (!r) r = consumeToken(b, "rdiv");
    if (!r) r = consumeToken(b, ">>");
    if (!r) r = consumeToken(b, "mod");
    if (!r) r = consumeToken(b, "rem");
    if (!r) r = consumeToken(b, "**");
    if (!r) r = consumeToken(b, "^");
    if (!r) r = consumeToken(b, "::");
    if (!r) r = consumeToken(b, "^^");
    if (!r) r = consumeToken(b, "<<");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // &valid_operator (
  //                             ':-'|'?-'|
  //                             'dynamic'|'discontiguous'|'initialization'|'meta_predicate'|'module_transparent'|'multifile'|
  //                             'public'|'thread_local'|'thread_initialization'|'volatile'|
  //                             '\+'|
  //                             '?'|
  //                             '+'|'-'|'\'|
  //                             '$'|
  //                             '@'|
  //                             '::'|
  //                             '^^'
  //                             )
  public static boolean known_left_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_left_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, KNOWN_LEFT_OPERATOR, "<known left operator>");
    r = known_left_operator_0(b, l + 1);
    r = r && known_left_operator_1(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // &valid_operator
  private static boolean known_left_operator_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_left_operator_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_);
    r = valid_operator(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // ':-'|'?-'|
  //                             'dynamic'|'discontiguous'|'initialization'|'meta_predicate'|'module_transparent'|'multifile'|
  //                             'public'|'thread_local'|'thread_initialization'|'volatile'|
  //                             '\+'|
  //                             '?'|
  //                             '+'|'-'|'\'|
  //                             '$'|
  //                             '@'|
  //                             '::'|
  //                             '^^'
  private static boolean known_left_operator_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_left_operator_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ":-");
    if (!r) r = consumeToken(b, "?-");
    if (!r) r = consumeToken(b, "dynamic");
    if (!r) r = consumeToken(b, "discontiguous");
    if (!r) r = consumeToken(b, "initialization");
    if (!r) r = consumeToken(b, "meta_predicate");
    if (!r) r = consumeToken(b, "module_transparent");
    if (!r) r = consumeToken(b, "multifile");
    if (!r) r = consumeToken(b, "public");
    if (!r) r = consumeToken(b, "thread_local");
    if (!r) r = consumeToken(b, "thread_initialization");
    if (!r) r = consumeToken(b, "volatile");
    if (!r) r = consumeToken(b, "\\+");
    if (!r) r = consumeToken(b, "?");
    if (!r) r = consumeToken(b, "+");
    if (!r) r = consumeToken(b, "-");
    if (!r) r = consumeToken(b, "\\");
    if (!r) r = consumeToken(b, "$");
    if (!r) r = consumeToken(b, "@");
    if (!r) r = consumeToken(b, "::");
    if (!r) r = consumeToken(b, "^^");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // LBRACKET term? RBRACKET
  public static boolean list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list")) return false;
    if (!nextTokenIs(b, LBRACKET)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LBRACKET);
    r = r && list_1(b, l + 1);
    r = r && consumeToken(b, RBRACKET);
    exit_section_(b, m, LIST, r);
    return r;
  }

  // term?
  private static boolean list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_1")) return false;
    term(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // item_*
  static boolean logtalkFile(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "logtalkFile")) return false;
    int c = current_position_(b);
    while (true) {
      if (!item_(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "logtalkFile", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // (braced_block|variable) MAP_OP (atom|INTEGER|variable)
  public static boolean map_reference(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_reference")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, MAP_REFERENCE, "<map reference>");
    r = map_reference_0(b, l + 1);
    r = r && consumeToken(b, MAP_OP);
    r = r && map_reference_2(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // braced_block|variable
  private static boolean map_reference_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_reference_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = braced_block(b, l + 1);
    if (!r) r = variable(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // atom|INTEGER|variable
  private static boolean map_reference_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_reference_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atom(b, l + 1);
    if (!r) r = consumeToken(b, INTEGER);
    if (!r) r = variable(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // basic_term known_binary_operator term
  public static boolean native_binary_operation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "native_binary_operation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, NATIVE_BINARY_OPERATION, "<native binary operation>");
    r = basic_term(b, l + 1);
    r = r && known_binary_operator(b, l + 1);
    r = r && term(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // known_left_operator term
  public static boolean native_left_operation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "native_left_operation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, NATIVE_LEFT_OPERATION, "<native left operation>");
    r = known_left_operator(b, l + 1);
    r = r && term(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // INTEGER|FLOAT|BIN_NUMBER|OCT_NUMBER|HEX_NUMBER|CHAR_CODE
  public static boolean number(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "number")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, NUMBER, "<number>");
    r = consumeToken(b, INTEGER);
    if (!r) r = consumeToken(b, FLOAT);
    if (!r) r = consumeToken(b, BIN_NUMBER);
    if (!r) r = consumeToken(b, OCT_NUMBER);
    if (!r) r = consumeToken(b, HEX_NUMBER);
    if (!r) r = consumeToken(b, CHAR_CODE);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // native_binary_operation|native_left_operation
  public static boolean operation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "operation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, OPERATION, "<operation>");
    r = native_binary_operation(b, l + 1);
    if (!r) r = native_left_operation(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // LPAREN term RPAREN
  public static boolean parenthesized_block(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_block")) return false;
    if (!nextTokenIs(b, LPAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, LPAREN);
    r = r && term(b, l + 1);
    r = r && consumeToken(b, RPAREN);
    exit_section_(b, m, PARENTHESIZED_BLOCK, r);
    return r;
  }

  /* ********************************************************** */
  // (operation|compound|atom) DOT
  public static boolean sentence(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sentence")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, SENTENCE, "<sentence>");
    r = sentence_0(b, l + 1);
    r = r && consumeToken(b, DOT);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // operation|compound|atom
  private static boolean sentence_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sentence_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = operation(b, l + 1);
    if (!r) r = compound(b, l + 1);
    if (!r) r = atom(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // operation|basic_term
  public static boolean term(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "term")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, TERM, "<term>");
    r = operation(b, l + 1);
    if (!r) r = basic_term(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // atom_keyword|UNQUOTED_ATOM|SYMBOLIC_ATOM
  public static boolean valid_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "valid_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, VALID_OPERATOR, "<valid operator>");
    r = atom_keyword(b, l + 1);
    if (!r) r = consumeToken(b, UNQUOTED_ATOM);
    if (!r) r = consumeToken(b, SYMBOLIC_ATOM);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // ANONYMOUS_VARIABLE|NAMED_VARIABLE
  public static boolean variable(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "variable")) return false;
    if (!nextTokenIs(b, "<variable>", ANONYMOUS_VARIABLE, NAMED_VARIABLE)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, VARIABLE, "<variable>");
    r = consumeToken(b, ANONYMOUS_VARIABLE);
    if (!r) r = consumeToken(b, NAMED_VARIABLE);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

}
