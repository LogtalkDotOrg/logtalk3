package org.logtalk.intellij.psi;


import static org.logtalk.intellij.psi.LogtalkTypes.KNOWN_BINARY_OPERATOR;
import static org.logtalk.intellij.psi.LogtalkTypes.KNOWN_LEFT_OPERATOR;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.logtalk.intellij.LogtalkLanguage;

import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

public class LogtalkElementType extends IElementType {

    public LogtalkElementType(@NotNull @NonNls String debugName) {
        super(debugName, LogtalkLanguage.INSTANCE);
    }

    public static IElementType getElementType(PsiElement element) {
        return element.getNode().getElementType();
    }

    public static boolean isParenthesis(IElementType tokenType) {
        return tokenType.equals(LogtalkTypes.LPAREN) || tokenType.equals(LogtalkTypes.RPAREN);
    }

    public static boolean isOperator(IElementType elementType) {
        return isKnownBinaryOperator(elementType) || isKnownLeftOperator(elementType);
    }

    public static boolean isKnownBinaryOperator(IElementType elementType) {
        return elementType.equals(KNOWN_BINARY_OPERATOR);
    }

    public static boolean isKnownLeftOperator(IElementType elementType) {
        return elementType.equals(KNOWN_LEFT_OPERATOR);
    }

    public static boolean isAtomKeyword(IElementType tokenType) {
        return tokenType.equals(LogtalkTypes.ATOM_KEYWORD);
    }

    public static boolean isCompoundNameKeyword(IElementType tokenType) {
        return tokenType.equals(LogtalkTypes.COMPOUND_NAME_KEYWORD);
    }

}
