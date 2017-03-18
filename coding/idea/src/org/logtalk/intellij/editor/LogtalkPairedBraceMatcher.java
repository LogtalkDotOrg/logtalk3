package org.logtalk.intellij.editor;

import static org.logtalk.intellij.psi.LogtalkTypes.LBRACE;
import static org.logtalk.intellij.psi.LogtalkTypes.LBRACKET;
import static org.logtalk.intellij.psi.LogtalkTypes.LPAREN;
import static org.logtalk.intellij.psi.LogtalkTypes.RBRACE;
import static org.logtalk.intellij.psi.LogtalkTypes.RBRACKET;
import static org.logtalk.intellij.psi.LogtalkTypes.RPAREN;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;


public class LogtalkPairedBraceMatcher implements PairedBraceMatcher {
    @Override
    public BracePair[] getPairs() {
        return new BracePair[] {
                new BracePair(LPAREN, RPAREN, true),
                new BracePair(LBRACKET, RBRACKET, false),
                new BracePair(LBRACE, RBRACE, false)
        };
    }

    @Override
    public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
        return true;
    }

    @Override
    public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
        return openingBraceOffset;
    }
}
