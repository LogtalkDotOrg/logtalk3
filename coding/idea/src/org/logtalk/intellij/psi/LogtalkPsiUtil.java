package org.logtalk.intellij.psi;


import static org.logtalk.intellij.psi.LogtalkTypes.KNOWN_BINARY_OPERATOR;
import static org.logtalk.intellij.psi.LogtalkTypes.KNOWN_LEFT_OPERATOR;

import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

public class LogtalkPsiUtil {

    public static IElementType getElementType(PsiElement element) {
        return element.getNode().getElementType();
    }

    public static boolean isOperator(PsiElement element) {
        return isKnownBinaryOperator(element) || isKnownLeftOperator(element);
    }

    public static boolean isKnownBinaryOperator(PsiElement element) {
        return getElementType(element).equals(KNOWN_BINARY_OPERATOR);
    }

    public static boolean isKnownLeftOperator(PsiElement element) {
        return getElementType(element).equals(KNOWN_LEFT_OPERATOR);
    }

    public static boolean isAtomKeyword(PsiElement element) {
        return getElementType(element).equals(LogtalkTypes.ATOM) && Constants.ATOM_KEYWORDS.contains(element.getText());
    }

    public static boolean isCompoundNameKeyword(PsiElement element) {
        return getElementType(element).equals(LogtalkTypes.COMPOUND_NAME) && Constants.COMPOUND_NAME_KEYWORDS.contains(element.getText());
    }
}
