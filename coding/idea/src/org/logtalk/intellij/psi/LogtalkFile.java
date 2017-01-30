package org.logtalk.intellij.psi;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.logtalk.intellij.LogtalkFileType;
import org.logtalk.intellij.LogtalkLanguage;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;

public class LogtalkFile extends PsiFileBase {

    public LogtalkFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, LogtalkLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return LogtalkFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "Logtalk File";
    }

    @Override
    public Icon getIcon(int flags) {
        return super.getIcon(flags);
    }

}
