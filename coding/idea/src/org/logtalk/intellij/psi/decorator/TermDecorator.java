package org.logtalk.intellij.psi.decorator;


import org.logtalk.intellij.psi.LogtalkBasicTerm;
import org.logtalk.intellij.psi.LogtalkTerm;

import com.intellij.psi.PsiElement;

public class TermDecorator extends PsiElementDecorator {

    public static boolean isTerm(PsiElement psiElement) {
        return psiElement instanceof LogtalkTerm;
    }

    public static boolean isBasicTerm(PsiElement psiElement) {
        return psiElement instanceof LogtalkBasicTerm;
    }

    private TermDecorator(PsiElement psiElement) {
        super(psiElement);
    }

    public TermDecorator termDecorator(PsiElement psiElement) {
        if (!isTerm(psiElement)) {
            throw new WrongPsiElementException(psiElement, LogtalkTerm.class);
        }
        return psiElement instanceof TermDecorator ? (TermDecorator) psiElement : new TermDecorator(psiElement);
    }

}
