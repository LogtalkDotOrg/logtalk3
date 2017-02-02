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
    else if (t == KNOWN_OPERATOR) {
      r = known_operator(b, 0);
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
  // QUOTED_ATOM|
  //             UNQUOTED_ATOM|
  //             KEYWORD_ATOM|
  //             DIRECTIVE_UNQUOTED_ATOM|
  //             DIRECTIVE_KEYWORD_ATOM|
  //             HEAD_UNQUOTED_ATOM|
  //             HEAD_KEYWORD_ATOM|
  //             CUT|
  //             CONS|
  //             known_operator
  public static boolean atom(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, ATOM, "<atom>");
    r = consumeToken(b, QUOTED_ATOM);
    if (!r) r = consumeToken(b, UNQUOTED_ATOM);
    if (!r) r = consumeToken(b, KEYWORD_ATOM);
    if (!r) r = consumeToken(b, DIRECTIVE_UNQUOTED_ATOM);
    if (!r) r = consumeToken(b, DIRECTIVE_KEYWORD_ATOM);
    if (!r) r = consumeToken(b, HEAD_UNQUOTED_ATOM);
    if (!r) r = consumeToken(b, HEAD_KEYWORD_ATOM);
    if (!r) r = consumeToken(b, CUT);
    if (!r) r = consumeToken(b, CONS);
    if (!r) r = known_operator(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // parenthesized_block|braced_block|list|map_reference|compound|atom|STRING|number|variable
  public static boolean basic_term(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "basic_term")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, BASIC_TERM, "<basic term>");
    r = parenthesized_block(b, l + 1);
    if (!r) r = braced_block(b, l + 1);
    if (!r) r = list(b, l + 1);
    if (!r) r = map_reference(b, l + 1);
    if (!r) r = compound(b, l + 1);
    if (!r) r = atom(b, l + 1);
    if (!r) r = consumeToken(b, STRING);
    if (!r) r = number(b, l + 1);
    if (!r) r = variable(b, l + 1);
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
  // QUOTED_COMPOUND_NAME|
  //                     UNQUOTED_COMPOUND_NAME|
  //                     KEYWORD_COMPOUND_NAME|
  //                     DIRECTIVE_UNQUOTED_COMPOUND_NAME|
  //                     DIRECTIVE_KEYWORD_COMPOUND_NAME|
  //                     HEAD_KEYWORD_COMPOUND_NAME|
  //                     HEAD_UNQUOTED_COMPOUND_NAME
  public static boolean compound_name(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compound_name")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, COMPOUND_NAME, "<compound name>");
    r = consumeToken(b, QUOTED_COMPOUND_NAME);
    if (!r) r = consumeToken(b, UNQUOTED_COMPOUND_NAME);
    if (!r) r = consumeToken(b, KEYWORD_COMPOUND_NAME);
    if (!r) r = consumeToken(b, DIRECTIVE_UNQUOTED_COMPOUND_NAME);
    if (!r) r = consumeToken(b, DIRECTIVE_KEYWORD_COMPOUND_NAME);
    if (!r) r = consumeToken(b, HEAD_KEYWORD_COMPOUND_NAME);
    if (!r) r = consumeToken(b, HEAD_UNQUOTED_COMPOUND_NAME);
    exit_section_(b, l, m, r, false, null);
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
  // LONG_ARROW_OP|
  //                             DIRECTIVE_OP|
  //                             SEMICOLON_OP|PIPE_OP|
  //                             IMPLICATION_OP|STAR_IMPLICATION_OP|
  //                             COMMA|
  //                             COLON_EQUALS_OP|
  //                             LESS_THAN_OP|EQUALS_OP|EQUALS_DOT_DOT_OP|EQUALS_AT_EQUALS_OP|BACKSLASH_EQUALS_AT_EQUALS_OP|
  //                             EQUALS_COLON_EQUALS_OP|LESS_OR_EQUALS_OP|DOUBLE_EQUALS_OP|EQUALS_BACKSLASH_EQUALS_OP|
  //                             GREATER_THAN_OP|GREATER_OR_EQUALS_OP|AT_LESS_THAN_OP|AT_EQUALS_LESS_THAN_OP|
  //                             AT_GREATER_THAN_OP|AT_GREATER_OR_EQUALS_OP|BACKSLASH_EQUALS_OP|BACKSLASH_DOUBLE_EQUALS_OP|
  //                             AS_OP|IS_OP|BUTTERFLY_OP|COLON_LESS_THAN_OP|
  //                             COLON_OP|
  //                             PLUS_OP|MINUS_OP|SLASH_BACKSLASH_OP|BACKSLASH_SLASH_OP|XOR_OP|
  //                             STAR_OP|SLASH_OP|DOUBLE_SLASH_OP|DIV_OP|RDIV_OP|DOUBLE_GREATER_THAN_OP|MOD_OP|REM_OP|
  //                             DOUBLE_STAR_OP|
  //                             CARET_OP|
  //                             MAP_OP|
  //                             DOLLAR_OP|
  //                             LGT_METHOD_CALL_OP|
  //                             LGT_SUPER_CALL_OP|
  //                             LGT_CONTEXT_SWITCHING_OP
  public static boolean known_binary_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_binary_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, KNOWN_BINARY_OPERATOR, "<known binary operator>");
    r = consumeToken(b, LONG_ARROW_OP);
    if (!r) r = consumeToken(b, DIRECTIVE_OP);
    if (!r) r = consumeToken(b, SEMICOLON_OP);
    if (!r) r = consumeToken(b, PIPE_OP);
    if (!r) r = consumeToken(b, IMPLICATION_OP);
    if (!r) r = consumeToken(b, STAR_IMPLICATION_OP);
    if (!r) r = consumeToken(b, COMMA);
    if (!r) r = consumeToken(b, COLON_EQUALS_OP);
    if (!r) r = consumeToken(b, LESS_THAN_OP);
    if (!r) r = consumeToken(b, EQUALS_OP);
    if (!r) r = consumeToken(b, EQUALS_DOT_DOT_OP);
    if (!r) r = consumeToken(b, EQUALS_AT_EQUALS_OP);
    if (!r) r = consumeToken(b, BACKSLASH_EQUALS_AT_EQUALS_OP);
    if (!r) r = consumeToken(b, EQUALS_COLON_EQUALS_OP);
    if (!r) r = consumeToken(b, LESS_OR_EQUALS_OP);
    if (!r) r = consumeToken(b, DOUBLE_EQUALS_OP);
    if (!r) r = consumeToken(b, EQUALS_BACKSLASH_EQUALS_OP);
    if (!r) r = consumeToken(b, GREATER_THAN_OP);
    if (!r) r = consumeToken(b, GREATER_OR_EQUALS_OP);
    if (!r) r = consumeToken(b, AT_LESS_THAN_OP);
    if (!r) r = consumeToken(b, AT_EQUALS_LESS_THAN_OP);
    if (!r) r = consumeToken(b, AT_GREATER_THAN_OP);
    if (!r) r = consumeToken(b, AT_GREATER_OR_EQUALS_OP);
    if (!r) r = consumeToken(b, BACKSLASH_EQUALS_OP);
    if (!r) r = consumeToken(b, BACKSLASH_DOUBLE_EQUALS_OP);
    if (!r) r = consumeToken(b, AS_OP);
    if (!r) r = consumeToken(b, IS_OP);
    if (!r) r = consumeToken(b, BUTTERFLY_OP);
    if (!r) r = consumeToken(b, COLON_LESS_THAN_OP);
    if (!r) r = consumeToken(b, COLON_OP);
    if (!r) r = consumeToken(b, PLUS_OP);
    if (!r) r = consumeToken(b, MINUS_OP);
    if (!r) r = consumeToken(b, SLASH_BACKSLASH_OP);
    if (!r) r = consumeToken(b, BACKSLASH_SLASH_OP);
    if (!r) r = consumeToken(b, XOR_OP);
    if (!r) r = consumeToken(b, STAR_OP);
    if (!r) r = consumeToken(b, SLASH_OP);
    if (!r) r = consumeToken(b, DOUBLE_SLASH_OP);
    if (!r) r = consumeToken(b, DIV_OP);
    if (!r) r = consumeToken(b, RDIV_OP);
    if (!r) r = consumeToken(b, DOUBLE_GREATER_THAN_OP);
    if (!r) r = consumeToken(b, MOD_OP);
    if (!r) r = consumeToken(b, REM_OP);
    if (!r) r = consumeToken(b, DOUBLE_STAR_OP);
    if (!r) r = consumeToken(b, CARET_OP);
    if (!r) r = consumeToken(b, MAP_OP);
    if (!r) r = consumeToken(b, DOLLAR_OP);
    if (!r) r = consumeToken(b, LGT_METHOD_CALL_OP);
    if (!r) r = consumeToken(b, LGT_SUPER_CALL_OP);
    if (!r) r = consumeToken(b, LGT_CONTEXT_SWITCHING_OP);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // DIRECTIVE_OP|
  //                             QUERY_OP|
  //                             DYNAMIC_OP|DISCONTIGUOUS_OP|INITIALIZATION_OP|META_PREDICATE_OP|MODULE_TRANSPARENT_OP|MULTIFILE_OP|
  //                             PUBLIC_OP|THREAD_LOCAL_OP|THREAD_INITIALIZATION_OP|VOLATILE_OP|
  //                             NOT_OP|
  //                             QUESTION_MARK_OP|
  //                             PLUS_OP|MINUS_OP|BACKSLASH_OP|
  //                             AT|
  //                             LGT_METHOD_CALL_OP|
  //                             LGT_SUPER_CALL_OP
  public static boolean known_left_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_left_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, KNOWN_LEFT_OPERATOR, "<known left operator>");
    r = consumeToken(b, DIRECTIVE_OP);
    if (!r) r = consumeToken(b, QUERY_OP);
    if (!r) r = consumeToken(b, DYNAMIC_OP);
    if (!r) r = consumeToken(b, DISCONTIGUOUS_OP);
    if (!r) r = consumeToken(b, INITIALIZATION_OP);
    if (!r) r = consumeToken(b, META_PREDICATE_OP);
    if (!r) r = consumeToken(b, MODULE_TRANSPARENT_OP);
    if (!r) r = consumeToken(b, MULTIFILE_OP);
    if (!r) r = consumeToken(b, PUBLIC_OP);
    if (!r) r = consumeToken(b, THREAD_LOCAL_OP);
    if (!r) r = consumeToken(b, THREAD_INITIALIZATION_OP);
    if (!r) r = consumeToken(b, VOLATILE_OP);
    if (!r) r = consumeToken(b, NOT_OP);
    if (!r) r = consumeToken(b, QUESTION_MARK_OP);
    if (!r) r = consumeToken(b, PLUS_OP);
    if (!r) r = consumeToken(b, MINUS_OP);
    if (!r) r = consumeToken(b, BACKSLASH_OP);
    if (!r) r = consumeToken(b, AT);
    if (!r) r = consumeToken(b, LGT_METHOD_CALL_OP);
    if (!r) r = consumeToken(b, LGT_SUPER_CALL_OP);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // known_binary_operator|known_left_operator
  public static boolean known_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "known_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, KNOWN_OPERATOR, "<known operator>");
    r = known_binary_operator(b, l + 1);
    if (!r) r = known_left_operator(b, l + 1);
    exit_section_(b, l, m, r, false, null);
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
  // native_binary_operation|native_left_operation|
  //                 custom_binary_operation|custom_left_operation
  public static boolean operation(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "operation")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, OPERATION, "<operation>");
    r = native_binary_operation(b, l + 1);
    if (!r) r = native_left_operation(b, l + 1);
    if (!r) r = custom_binary_operation(b, l + 1);
    if (!r) r = custom_left_operation(b, l + 1);
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
