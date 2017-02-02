package org.logtalk.intellij.psi.decorator;


import org.logtalk.intellij.psi.LogtalkAtom;

import com.intellij.psi.PsiElement;

public class AtomDecorator extends PsiElementDecorator {

    public static boolean isAtom(PsiElement psiElement) {
        return psiElement instanceof LogtalkAtom;
    }

    private AtomDecorator(PsiElement psiElement) {
        super(psiElement);
    }

    public static AtomDecorator atomDecorator(PsiElement psiElement) {
        if (!isAtom(psiElement)) {
            throw new WrongPsiElementException(psiElement, LogtalkAtom.class);
        }
        return psiElement instanceof AtomDecorator ? (AtomDecorator) psiElement : new AtomDecorator(psiElement);
    }

/*    public static AtomDecorator fromTerm(PsiElement psiElement) {
        if (isBasicTerm(psiElement)) {
            return fromBasicTerm(psiElement);
        } else if (isTerm(psiElement)) {
            return fromBasicTerm(psiElement.getFirstChild());
        } else {
            throw new WrongPsiElementException(psiElement, LogtalkBasicTerm.class, LogtalkTerm.class);
        }
    }*/

    private static AtomDecorator fromBasicTerm(PsiElement psiElement) {
        return atomDecorator(psiElement.getFirstChild());
    }

    public String getAtomText() {
        return getText();
    }

}
