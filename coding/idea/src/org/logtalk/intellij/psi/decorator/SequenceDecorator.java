package org.logtalk.intellij.psi.decorator;


import static org.logtalk.intellij.psi.decorator.OperationDecorator.isOperation;
import static org.logtalk.intellij.psi.decorator.OperationDecorator.operationDecorator;

import java.util.ArrayList;
import java.util.List;

import com.intellij.psi.PsiElement;

public class SequenceDecorator extends PsiElementDecorator {

    private static final String SEQUENCE_SEPARATOR = ",";

    public static boolean isSequence(PsiElement psiElement) {
        return isOperation(psiElement) &&
                operationDecorator(psiElement).getOperatorText().equals(SEQUENCE_SEPARATOR) &&
                psiElement.getChildren().length <= 2;
    }

    private SequenceDecorator(PsiElement psiElement) {
        super(psiElement);
    }

    public static SequenceDecorator sequenceDecorator(PsiElement psiElement) {
        if (!isSequence(psiElement)) {
            throw new WrongPsiElementException(psiElement, "Sequence");
        }
        return psiElement instanceof SequenceDecorator ? (SequenceDecorator) psiElement : new SequenceDecorator(psiElement);
    }

    public int size() {
        return getMembers().size();
    }

    public List<PsiElement> getMembers() {
        List<PsiElement> members = new ArrayList<>();
        PsiElement sequence = getDecoratedPsiElement();
        while (sequence != null) {
            PsiElement firstSequenceChild = getFirstSequenceChild(sequence);
            members.add(firstSequenceChild);
            PsiElement secondSequenceChild = getSecondSequenceChild(sequence);
            if (isSequence(secondSequenceChild.getFirstChild())) {
                sequence = secondSequenceChild.getFirstChild();
            } else {
                sequence = null;
                members.add(secondSequenceChild);
            }
        }
        return members;
    }

    private PsiElement getFirstSequenceChild(PsiElement psiElement) {
        return psiElement.getFirstChild().getFirstChild();
    }

    private PsiElement getSecondSequenceChild(PsiElement psiElement) {
        return psiElement.getFirstChild().getLastChild();
    }
}
