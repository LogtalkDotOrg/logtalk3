// This is a generated file. Not intended for manual editing.
package org.logtalk.intellij.psi;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import org.logtalk.intellij.psi.impl.*;

public interface LogtalkTypes {

  IElementType ATOM = new LogtalkElementType("ATOM");
  IElementType BASIC_TERM = new LogtalkElementType("BASIC_TERM");
  IElementType BRACED_BLOCK = new LogtalkElementType("BRACED_BLOCK");
  IElementType COMPOUND = new LogtalkElementType("COMPOUND");
  IElementType COMPOUND_NAME = new LogtalkElementType("COMPOUND_NAME");
  IElementType CUSTOM_BINARY_OPERATION = new LogtalkElementType("CUSTOM_BINARY_OPERATION");
  IElementType CUSTOM_LEFT_OPERATION = new LogtalkElementType("CUSTOM_LEFT_OPERATION");
  IElementType KNOWN_BINARY_OPERATOR = new LogtalkElementType("KNOWN_BINARY_OPERATOR");
  IElementType KNOWN_LEFT_OPERATOR = new LogtalkElementType("KNOWN_LEFT_OPERATOR");
  IElementType KNOWN_OPERATOR = new LogtalkElementType("KNOWN_OPERATOR");
  IElementType LIST = new LogtalkElementType("LIST");
  IElementType MAP_REFERENCE = new LogtalkElementType("MAP_REFERENCE");
  IElementType NATIVE_BINARY_OPERATION = new LogtalkElementType("NATIVE_BINARY_OPERATION");
  IElementType NATIVE_LEFT_OPERATION = new LogtalkElementType("NATIVE_LEFT_OPERATION");
  IElementType NUMBER = new LogtalkElementType("NUMBER");
  IElementType OPERATION = new LogtalkElementType("OPERATION");
  IElementType PARENTHESIZED_BLOCK = new LogtalkElementType("PARENTHESIZED_BLOCK");
  IElementType SENTENCE = new LogtalkElementType("SENTENCE");
  IElementType TERM = new LogtalkElementType("TERM");
  IElementType VARIABLE = new LogtalkElementType("VARIABLE");

  IElementType ANONYMOUS_VARIABLE = new LogtalkTokenType("ANONYMOUS_VARIABLE");
  IElementType AS_OP = new LogtalkTokenType("AS_OP");
  IElementType AT = new LogtalkTokenType("AT");
  IElementType AT_EQUALS_LESS_THAN_OP = new LogtalkTokenType("AT_EQUALS_LESS_THAN_OP");
  IElementType AT_GREATER_OR_EQUALS_OP = new LogtalkTokenType("AT_GREATER_OR_EQUALS_OP");
  IElementType AT_GREATER_THAN_OP = new LogtalkTokenType("AT_GREATER_THAN_OP");
  IElementType AT_LESS_THAN_OP = new LogtalkTokenType("AT_LESS_THAN_OP");
  IElementType BACKSLASH_DOUBLE_EQUALS_OP = new LogtalkTokenType("BACKSLASH_DOUBLE_EQUALS_OP");
  IElementType BACKSLASH_EQUALS_AT_EQUALS_OP = new LogtalkTokenType("BACKSLASH_EQUALS_AT_EQUALS_OP");
  IElementType BACKSLASH_EQUALS_OP = new LogtalkTokenType("BACKSLASH_EQUALS_OP");
  IElementType BACKSLASH_OP = new LogtalkTokenType("BACKSLASH_OP");
  IElementType BACKSLASH_SLASH_OP = new LogtalkTokenType("BACKSLASH_SLASH_OP");
  IElementType BIN_NUMBER = new LogtalkTokenType("BIN_NUMBER");
  IElementType BUTTERFLY_OP = new LogtalkTokenType("BUTTERFLY_OP");
  IElementType CARET_OP = new LogtalkTokenType("CARET_OP");
  IElementType CHAR_CODE = new LogtalkTokenType("CHAR_CODE");
  IElementType COLON_EQUALS_OP = new LogtalkTokenType("COLON_EQUALS_OP");
  IElementType COLON_LESS_THAN_OP = new LogtalkTokenType("COLON_LESS_THAN_OP");
  IElementType COLON_OP = new LogtalkTokenType("COLON_OP");
  IElementType COMMA = new LogtalkTokenType("COMMA");
  IElementType COMMENT = new LogtalkTokenType("COMMENT");
  IElementType CONS = new LogtalkTokenType("CONS");
  IElementType CRLF = new LogtalkTokenType("CRLF");
  IElementType CUT = new LogtalkTokenType("CUT");
  IElementType DIRECTIVE_KEYWORD_ATOM = new LogtalkTokenType("DIRECTIVE_KEYWORD_ATOM");
  IElementType DIRECTIVE_KEYWORD_COMPOUND_NAME = new LogtalkTokenType("DIRECTIVE_KEYWORD_COMPOUND_NAME");
  IElementType DIRECTIVE_OP = new LogtalkTokenType("DIRECTIVE_OP");
  IElementType DIRECTIVE_UNQUOTED_ATOM = new LogtalkTokenType("DIRECTIVE_UNQUOTED_ATOM");
  IElementType DIRECTIVE_UNQUOTED_COMPOUND_NAME = new LogtalkTokenType("DIRECTIVE_UNQUOTED_COMPOUND_NAME");
  IElementType DISCONTIGUOUS_OP = new LogtalkTokenType("DISCONTIGUOUS_OP");
  IElementType DIV_OP = new LogtalkTokenType("DIV_OP");
  IElementType DOLLAR_OP = new LogtalkTokenType("DOLLAR_OP");
  IElementType DOT = new LogtalkTokenType("DOT");
  IElementType DOUBLE_EQUALS_OP = new LogtalkTokenType("DOUBLE_EQUALS_OP");
  IElementType DOUBLE_GREATER_THAN_OP = new LogtalkTokenType("DOUBLE_GREATER_THAN_OP");
  IElementType DOUBLE_SLASH_OP = new LogtalkTokenType("DOUBLE_SLASH_OP");
  IElementType DOUBLE_STAR_OP = new LogtalkTokenType("DOUBLE_STAR_OP");
  IElementType DYNAMIC_OP = new LogtalkTokenType("DYNAMIC_OP");
  IElementType EQUALS_AT_EQUALS_OP = new LogtalkTokenType("EQUALS_AT_EQUALS_OP");
  IElementType EQUALS_BACKSLASH_EQUALS_OP = new LogtalkTokenType("EQUALS_BACKSLASH_EQUALS_OP");
  IElementType EQUALS_COLON_EQUALS_OP = new LogtalkTokenType("EQUALS_COLON_EQUALS_OP");
  IElementType EQUALS_DOT_DOT_OP = new LogtalkTokenType("EQUALS_DOT_DOT_OP");
  IElementType EQUALS_OP = new LogtalkTokenType("EQUALS_OP");
  IElementType FLOAT = new LogtalkTokenType("FLOAT");
  IElementType GREATER_OR_EQUALS_OP = new LogtalkTokenType("GREATER_OR_EQUALS_OP");
  IElementType GREATER_THAN_OP = new LogtalkTokenType("GREATER_THAN_OP");
  IElementType HEAD_KEYWORD_ATOM = new LogtalkTokenType("HEAD_KEYWORD_ATOM");
  IElementType HEAD_KEYWORD_COMPOUND_NAME = new LogtalkTokenType("HEAD_KEYWORD_COMPOUND_NAME");
  IElementType HEAD_UNQUOTED_ATOM = new LogtalkTokenType("HEAD_UNQUOTED_ATOM");
  IElementType HEAD_UNQUOTED_COMPOUND_NAME = new LogtalkTokenType("HEAD_UNQUOTED_COMPOUND_NAME");
  IElementType HEX_NUMBER = new LogtalkTokenType("HEX_NUMBER");
  IElementType IMPLICATION_OP = new LogtalkTokenType("IMPLICATION_OP");
  IElementType INITIALIZATION_OP = new LogtalkTokenType("INITIALIZATION_OP");
  IElementType INTEGER = new LogtalkTokenType("INTEGER");
  IElementType IS_OP = new LogtalkTokenType("IS_OP");
  IElementType KEYWORD_ATOM = new LogtalkTokenType("KEYWORD_ATOM");
  IElementType KEYWORD_COMPOUND_NAME = new LogtalkTokenType("KEYWORD_COMPOUND_NAME");
  IElementType LBRACE = new LogtalkTokenType("LBRACE");
  IElementType LBRACKET = new LogtalkTokenType("LBRACKET");
  IElementType LESS_OR_EQUALS_OP = new LogtalkTokenType("LESS_OR_EQUALS_OP");
  IElementType LESS_THAN_OP = new LogtalkTokenType("LESS_THAN_OP");
  IElementType LGT_CONTEXT_SWITCHING_OP = new LogtalkTokenType("LGT_CONTEXT_SWITCHING_OP");
  IElementType LGT_METHOD_CALL_OP = new LogtalkTokenType("LGT_METHOD_CALL_OP");
  IElementType LGT_SUPER_CALL_OP = new LogtalkTokenType("LGT_SUPER_CALL_OP");
  IElementType LONG_ARROW_OP = new LogtalkTokenType("LONG_ARROW_OP");
  IElementType LPAREN = new LogtalkTokenType("LPAREN");
  IElementType MAP_OP = new LogtalkTokenType("MAP_OP");
  IElementType META_PREDICATE_OP = new LogtalkTokenType("META_PREDICATE_OP");
  IElementType MINUS_OP = new LogtalkTokenType("MINUS_OP");
  IElementType MODULE_TRANSPARENT_OP = new LogtalkTokenType("MODULE_TRANSPARENT_OP");
  IElementType MOD_OP = new LogtalkTokenType("MOD_OP");
  IElementType MULTIFILE_OP = new LogtalkTokenType("MULTIFILE_OP");
  IElementType NAMED_VARIABLE = new LogtalkTokenType("NAMED_VARIABLE");
  IElementType NOT_OP = new LogtalkTokenType("NOT_OP");
  IElementType OCT_NUMBER = new LogtalkTokenType("OCT_NUMBER");
  IElementType PIPE_OP = new LogtalkTokenType("PIPE_OP");
  IElementType PLUS_OP = new LogtalkTokenType("PLUS_OP");
  IElementType PUBLIC_OP = new LogtalkTokenType("PUBLIC_OP");
  IElementType QUERY_OP = new LogtalkTokenType("QUERY_OP");
  IElementType QUESTION_MARK_OP = new LogtalkTokenType("QUESTION_MARK_OP");
  IElementType QUOTED_ATOM = new LogtalkTokenType("QUOTED_ATOM");
  IElementType QUOTED_COMPOUND_NAME = new LogtalkTokenType("QUOTED_COMPOUND_NAME");
  IElementType RBRACE = new LogtalkTokenType("RBRACE");
  IElementType RBRACKET = new LogtalkTokenType("RBRACKET");
  IElementType RDIV_OP = new LogtalkTokenType("RDIV_OP");
  IElementType REM_OP = new LogtalkTokenType("REM_OP");
  IElementType RPAREN = new LogtalkTokenType("RPAREN");
  IElementType SEMICOLON_OP = new LogtalkTokenType("SEMICOLON_OP");
  IElementType SLASH_BACKSLASH_OP = new LogtalkTokenType("SLASH_BACKSLASH_OP");
  IElementType SLASH_OP = new LogtalkTokenType("SLASH_OP");
  IElementType STAR_IMPLICATION_OP = new LogtalkTokenType("STAR_IMPLICATION_OP");
  IElementType STAR_OP = new LogtalkTokenType("STAR_OP");
  IElementType STRING = new LogtalkTokenType("STRING");
  IElementType THREAD_INITIALIZATION_OP = new LogtalkTokenType("THREAD_INITIALIZATION_OP");
  IElementType THREAD_LOCAL_OP = new LogtalkTokenType("THREAD_LOCAL_OP");
  IElementType UNQUOTED_ATOM = new LogtalkTokenType("UNQUOTED_ATOM");
  IElementType UNQUOTED_COMPOUND_NAME = new LogtalkTokenType("UNQUOTED_COMPOUND_NAME");
  IElementType VOLATILE_OP = new LogtalkTokenType("VOLATILE_OP");
  IElementType XOR_OP = new LogtalkTokenType("XOR_OP");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == ATOM) {
        return new LogtalkAtomImpl(node);
      }
      else if (type == BASIC_TERM) {
        return new LogtalkBasicTermImpl(node);
      }
      else if (type == BRACED_BLOCK) {
        return new LogtalkBracedBlockImpl(node);
      }
      else if (type == COMPOUND) {
        return new LogtalkCompoundImpl(node);
      }
      else if (type == COMPOUND_NAME) {
        return new LogtalkCompoundNameImpl(node);
      }
      else if (type == CUSTOM_BINARY_OPERATION) {
        return new LogtalkCustomBinaryOperationImpl(node);
      }
      else if (type == CUSTOM_LEFT_OPERATION) {
        return new LogtalkCustomLeftOperationImpl(node);
      }
      else if (type == KNOWN_BINARY_OPERATOR) {
        return new LogtalkKnownBinaryOperatorImpl(node);
      }
      else if (type == KNOWN_LEFT_OPERATOR) {
        return new LogtalkKnownLeftOperatorImpl(node);
      }
      else if (type == KNOWN_OPERATOR) {
        return new LogtalkKnownOperatorImpl(node);
      }
      else if (type == LIST) {
        return new LogtalkListImpl(node);
      }
      else if (type == MAP_REFERENCE) {
        return new LogtalkMapReferenceImpl(node);
      }
      else if (type == NATIVE_BINARY_OPERATION) {
        return new LogtalkNativeBinaryOperationImpl(node);
      }
      else if (type == NATIVE_LEFT_OPERATION) {
        return new LogtalkNativeLeftOperationImpl(node);
      }
      else if (type == NUMBER) {
        return new LogtalkNumberImpl(node);
      }
      else if (type == OPERATION) {
        return new LogtalkOperationImpl(node);
      }
      else if (type == PARENTHESIZED_BLOCK) {
        return new LogtalkParenthesizedBlockImpl(node);
      }
      else if (type == SENTENCE) {
        return new LogtalkSentenceImpl(node);
      }
      else if (type == TERM) {
        return new LogtalkTermImpl(node);
      }
      else if (type == VARIABLE) {
        return new LogtalkVariableImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
