package org.logtalk.intellij.editor;


import static org.logtalk.intellij.ast.decorator.CommentDecorator.isComment;
import static org.logtalk.intellij.psi.decorator.ListDecorator.isList;
import static org.logtalk.intellij.psi.decorator.ListDecorator.listDecorator;
import static org.logtalk.intellij.psi.decorator.SentenceDecorator.isSentence;

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;

public class LogtalkFoldingBuilder implements FoldingBuilder {

    public static final int LIST_SIZE_FOLDING_THRESHOLD = 3;

    @NotNull
    @Override
    public FoldingDescriptor[] buildFoldRegions(@NotNull ASTNode node, @NotNull Document document) {
        final List<FoldingDescriptor> descriptors = new ArrayList<>();
        collectDescriptorsRecursively(node, document, descriptors);
        return descriptors.toArray(new FoldingDescriptor[descriptors.size()]);
    }

    private static void collectDescriptorsRecursively(@NotNull ASTNode node,
                                                      @NotNull Document document,
                                                      @NotNull List<FoldingDescriptor> descriptors) {
        if (isFoldable(node, document)) {
            descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
        }
        for (ASTNode child : node.getChildren(null)) {
            collectDescriptorsRecursively(child, document, descriptors);
        }
    }

    private static boolean isFoldable(ASTNode node, Document document) {
        PsiElement psi = node.getPsi();
        if ( isComment(node) || isSentence(psi) || isList(psi)
          && spanMultipleLines(node, document)) {
            return true;
        }

        //TODO fixme
        //folding of single-line lists containing more elements than LIST_SIZE_FOLDING_THRESHOLD is not working
        if (isList(psi) && listDecorator(psi).size() > LIST_SIZE_FOLDING_THRESHOLD) {
            return true;
        }
        return false;
    }

    @Nullable
    @Override
    public String getPlaceholderText(@NotNull ASTNode node) {
        PsiElement psi = node.getPsi();
        if (isComment(node)) {
            return "/*...*/";
        } else if (isList(psi)) {
            return "[...]";
        } /*else if (isSentence(psi)) {
            if (isOperation(psi.getFirstChild())) {
                OperationDecorator operation = operationDecorator(psi.getFirstChild());
                if (operation.isRule() || operation.isGrammarRule()) {
                    *//*String head = operation.getFirstChild().getFirstChild().getText();
                    return head + " " + operation.getOperatorText() + " ...";*//*
                } else if (operation.isDirective()) {
                    //return operation.getFirstChild().getChildren()[1].getFirstChild().getText() + ".";
                }
            }
        }*/
        //return "...";
        return psi.getText();
    }

    @Override
    public boolean isCollapsedByDefault(@NotNull ASTNode node) {
        return false;
    }


    private static boolean spanMultipleLines(@NotNull ASTNode node, @NotNull Document document) {
        final TextRange range = node.getTextRange();
        return document.getLineNumber(range.getStartOffset()) < document.getLineNumber(range.getEndOffset());
    }


}
