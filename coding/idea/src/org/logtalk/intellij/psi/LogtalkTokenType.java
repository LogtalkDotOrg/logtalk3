package org.logtalk.intellij.psi;


import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.logtalk.intellij.LogtalkLanguage;

import com.intellij.psi.tree.IElementType;

public class LogtalkTokenType extends IElementType {

    public LogtalkTokenType(@NotNull @NonNls String debugName) {
        super(debugName, LogtalkLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "LogtalkTokenType." + super.toString();
    }
}
