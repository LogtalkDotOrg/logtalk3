package org.logtalk.intellij.ast.decorator;

import org.logtalk.intellij.psi.LogtalkTypes;

import com.intellij.lang.ASTNode;


public class CommentDecorator extends AstNodeDecorator {

    public static boolean isComment(ASTNode node) {
        return node.getElementType() == LogtalkTypes.COMMENT;
    }

    private CommentDecorator(ASTNode node) {
        super(node);
    }

    public static CommentDecorator commentDecorator(ASTNode node) {
        if (!isComment(node)) {
            throw new WrongAstNodeException(node.getElementType(), LogtalkTypes.COMMENT);
        }
        return node instanceof CommentDecorator ? (CommentDecorator) node : new CommentDecorator(node);
    }

}
