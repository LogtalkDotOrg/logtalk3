package org.logtalk.intellij.psi.decorator;


import org.logtalk.intellij.psi.LogtalkCustomBinaryOperation;
import org.logtalk.intellij.psi.LogtalkNativeBinaryOperation;
import org.logtalk.intellij.psi.LogtalkOperation;

import com.intellij.psi.PsiElement;

public class OperationDecorator extends PsiElementDecorator {

    private static final String RULE_OPERATOR = ":-";
    private static final String GRAMMAR__RULE_OPERATOR = "-->";
    private static final String DIRECTIVE_OPERATOR = RULE_OPERATOR;

    public static boolean isOperation(PsiElement psiElement) {
        return psiElement instanceof LogtalkOperation;
    }

    private OperationDecorator(PsiElement psiElement) {
        super(psiElement);
    }

    public static OperationDecorator operationDecorator(PsiElement psiElement) {
        if (!isOperation(psiElement)) {
            throw new WrongPsiElementException(psiElement, LogtalkOperation.class);
        }
        return psiElement instanceof OperationDecorator ? (OperationDecorator) psiElement : new OperationDecorator(psiElement);
    }

    public boolean isBinary() {
        return getFirstChild() instanceof LogtalkNativeBinaryOperation ||
                getFirstChild() instanceof LogtalkCustomBinaryOperation;
    }

    public boolean isLeft() {
        return !isBinary();
    }

    private PsiElement getOperator() {
        if (isLeft()) {
            return getFirstChild().getFirstChild();
        } else {
            return getFirstChild().getChildren()[1];
        }
    }

    public String getOperatorSymbol() {
        return getOperator().getText();
    }

    public boolean isDirective() {
        return isLeft() && getOperatorSymbol().equals(DIRECTIVE_OPERATOR);
    }


    public boolean isRule() {
        return isBinary() && getOperatorSymbol().equals(RULE_OPERATOR);
    }

    public boolean isGrammarRule() {
        return isBinary() && getOperatorSymbol().equals(GRAMMAR__RULE_OPERATOR);
    }

}
