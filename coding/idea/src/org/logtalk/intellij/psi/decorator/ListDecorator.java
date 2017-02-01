package org.logtalk.intellij.psi.decorator;


import static org.logtalk.intellij.psi.decorator.SequenceDecorator.isSequence;
import static org.logtalk.intellij.psi.decorator.SequenceDecorator.sequenceDecorator;

import java.util.Optional;

import org.logtalk.intellij.psi.LogtalkList;

import com.intellij.psi.PsiElement;

public class ListDecorator extends PsiElementDecorator {

    public static boolean isList(PsiElement psiElement) {
        return psiElement instanceof LogtalkList;
    }

    private final Optional<SequenceDecorator> optSequence;
    private final Optional<PsiElement> optSingleton;

    private ListDecorator(PsiElement psiElement) {
        super(psiElement);
        SequenceDecorator nestedSequence = null;
        PsiElement singleton = null;

        if (psiElement.getChildren().length == 3) {
            PsiElement middleNode = psiElement.getChildren()[1];
            if (isSequence(middleNode.getFirstChild())) {
                nestedSequence = sequenceDecorator(middleNode.getFirstChild());
            } else {
                singleton = middleNode;
            }
        }
        optSequence = Optional.ofNullable(nestedSequence);
        optSingleton = Optional.ofNullable(singleton);
    }

    public static ListDecorator listDecorator(PsiElement psiElement) {
        if (!isList(psiElement)) {
            throw new WrongPsiElementException(psiElement, LogtalkList.class);
        }
        return psiElement instanceof ListDecorator ? (ListDecorator) psiElement : new ListDecorator(psiElement);
    }

    public int size() {
        if (optSequence.isPresent()) {
            return optSequence.get().size();
        } else if (optSingleton.isPresent()){
            return 1;
        } else {
            return 0;
        }
    }

}
