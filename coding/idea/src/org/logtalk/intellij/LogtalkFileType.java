package org.logtalk.intellij;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.intellij.openapi.fileTypes.LanguageFileType;

public class LogtalkFileType extends LanguageFileType {

    public static final LogtalkFileType INSTANCE = new LogtalkFileType();

    private LogtalkFileType() {
        super(LogtalkLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Logtalk resource";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "A Logtalk resource.";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "lgt";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return Icons.LOGTALK;
    }

}
