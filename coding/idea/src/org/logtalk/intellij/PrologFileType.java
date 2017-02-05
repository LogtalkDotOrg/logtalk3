package org.logtalk.intellij;


import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.intellij.openapi.fileTypes.LanguageFileType;

public class PrologFileType extends LanguageFileType {

    public static final PrologFileType INSTANCE = new PrologFileType();

    private PrologFileType() {
        super(PrologLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Prolog resource";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Prolog";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "pl";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return Icons.PROLOG;
    }

}
