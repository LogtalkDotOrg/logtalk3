package org.logtalk.intellij;


import org.logtalk.intellij.psi.PrologFile;

import com.intellij.lang.Language;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IFileElementType;

public class PrologParserDefinition extends LogicParserDefinition {

    public static final IFileElementType FILE =
            new IFileElementType(Language.findInstance(PrologLanguage.class));

    @Override
    public IFileElementType getFileNodeType() {
        return FILE;
    }

    @Override
    public PsiFile createFile(FileViewProvider viewProvider) {
        return new PrologFile(viewProvider);
    }
}
