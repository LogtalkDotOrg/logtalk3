// This is a generated file. Not intended for manual editing.
package org.logtalk.intellij.psi;

import org.logtalk.intellij.psi.impl.LogtalkAtomImpl;
import org.logtalk.intellij.psi.impl.LogtalkBasicTermImpl;
import org.logtalk.intellij.psi.impl.LogtalkBracedBlockImpl;
import org.logtalk.intellij.psi.impl.LogtalkCompoundImpl;
import org.logtalk.intellij.psi.impl.LogtalkCompoundNameImpl;
import org.logtalk.intellij.psi.impl.LogtalkCustomBinaryOperationImpl;
import org.logtalk.intellij.psi.impl.LogtalkCustomLeftOperationImpl;
import org.logtalk.intellij.psi.impl.LogtalkKnownBinaryOperatorImpl;
import org.logtalk.intellij.psi.impl.LogtalkKnownLeftOperatorImpl;
import org.logtalk.intellij.psi.impl.LogtalkListImpl;
import org.logtalk.intellij.psi.impl.LogtalkMapReferenceImpl;
import org.logtalk.intellij.psi.impl.LogtalkNativeBinaryOperationImpl;
import org.logtalk.intellij.psi.impl.LogtalkNativeLeftOperationImpl;
import org.logtalk.intellij.psi.impl.LogtalkNumberImpl;
import org.logtalk.intellij.psi.impl.LogtalkOperationImpl;
import org.logtalk.intellij.psi.impl.LogtalkOperatorImpl;
import org.logtalk.intellij.psi.impl.LogtalkParenthesizedBlockImpl;
import org.logtalk.intellij.psi.impl.LogtalkSentenceImpl;
import org.logtalk.intellij.psi.impl.LogtalkTermImpl;
import org.logtalk.intellij.psi.impl.LogtalkValidOperatorImpl;
import org.logtalk.intellij.psi.impl.LogtalkVariableImpl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

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
  IElementType LIST = new LogtalkElementType("LIST");
  IElementType MAP_REFERENCE = new LogtalkElementType("MAP_REFERENCE");
  IElementType NATIVE_BINARY_OPERATION = new LogtalkElementType("NATIVE_BINARY_OPERATION");
  IElementType NATIVE_LEFT_OPERATION = new LogtalkElementType("NATIVE_LEFT_OPERATION");
  IElementType NUMBER = new LogtalkElementType("NUMBER");
  IElementType OPERATION = new LogtalkElementType("OPERATION");
  IElementType OPERATOR = new LogtalkElementType("OPERATOR");
  IElementType PARENTHESIZED_BLOCK = new LogtalkElementType("PARENTHESIZED_BLOCK");
  IElementType SENTENCE = new LogtalkElementType("SENTENCE");
  IElementType TERM = new LogtalkElementType("TERM");
  IElementType VALID_OPERATOR = new LogtalkElementType("VALID_OPERATOR");
  IElementType VARIABLE = new LogtalkElementType("VARIABLE");

  IElementType ANONYMOUS_VARIABLE = new LogtalkTokenType("ANONYMOUS_VARIABLE");
  IElementType BIN_NUMBER = new LogtalkTokenType("BIN_NUMBER");
  IElementType CHAR_CODE = new LogtalkTokenType("CHAR_CODE");
  IElementType COMMENT = new LogtalkTokenType("COMMENT");
  IElementType CRLF = new LogtalkTokenType("CRLF");
  IElementType DOT = new LogtalkTokenType("DOT");
  IElementType FLOAT = new LogtalkTokenType("FLOAT");
  IElementType HEX_NUMBER = new LogtalkTokenType("HEX_NUMBER");
  IElementType INTEGER = new LogtalkTokenType("INTEGER");
  IElementType LBRACE = new LogtalkTokenType("LBRACE");
  IElementType LBRACKET = new LogtalkTokenType("LBRACKET");
  IElementType LPAREN = new LogtalkTokenType("LPAREN");
  IElementType MAP_OP = new LogtalkTokenType("MAP_OP");
  IElementType NAMED_VARIABLE = new LogtalkTokenType("NAMED_VARIABLE");
  IElementType OCT_NUMBER = new LogtalkTokenType("OCT_NUMBER");
  IElementType QUOTED_ATOM = new LogtalkTokenType("QUOTED_ATOM");
  IElementType QUOTED_COMPOUND_NAME = new LogtalkTokenType("QUOTED_COMPOUND_NAME");
  IElementType RBRACE = new LogtalkTokenType("RBRACE");
  IElementType RBRACKET = new LogtalkTokenType("RBRACKET");
  IElementType RPAREN = new LogtalkTokenType("RPAREN");
  IElementType STRING = new LogtalkTokenType("STRING");
  IElementType SYMBOLIC_ATOM = new LogtalkTokenType("SYMBOLIC_ATOM");
  IElementType UNQUOTED_ATOM = new LogtalkTokenType("UNQUOTED_ATOM");
  IElementType UNQUOTED_COMPOUND_NAME = new LogtalkTokenType("UNQUOTED_COMPOUND_NAME");

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
      else if (type == OPERATOR) {
        return new LogtalkOperatorImpl(node);
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
      else if (type == VALID_OPERATOR) {
        return new LogtalkValidOperatorImpl(node);
      }
      else if (type == VARIABLE) {
        return new LogtalkVariableImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
