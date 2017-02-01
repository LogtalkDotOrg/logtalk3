package org.logtalk.intellij.psi;


import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.logtalk.intellij.LogtalkLanguage;

import com.intellij.psi.tree.IElementType;

public class LogtalkElementType extends IElementType {

    public LogtalkElementType(@NotNull @NonNls String debugName) {
        super(debugName, LogtalkLanguage.INSTANCE);
    }

}