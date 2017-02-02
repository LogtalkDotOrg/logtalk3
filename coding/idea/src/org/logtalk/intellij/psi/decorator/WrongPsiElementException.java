package org.logtalk.intellij.psi.decorator;


import com.intellij.psi.PsiElement;

public class WrongPsiElementException extends RuntimeException {

    private final PsiElement actual;
    private final String expected;
    //private final List<Class<? extends PsiElement>> expected;


    public WrongPsiElementException(PsiElement actual, Class<? extends PsiElement>... expected) {
        this(actual, expected.toString());
    }

    public WrongPsiElementException(PsiElement actual, String expected) {
        this.actual = actual;
        this.expected = expected;
    }

    public PsiElement getActual() {
        return actual;
    }

    public String getExpected() {
        return expected;
    }

    @Override
    public String getMessage() {
        return expected + " expected. Actual: " + actual;
    }

}
