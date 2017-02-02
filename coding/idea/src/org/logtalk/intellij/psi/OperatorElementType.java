package org.logtalk.intellij.psi;


import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class OperatorElementType extends LogtalkElementType {
    public OperatorElementType(@NotNull @NonNls final String debugName) {
        super(debugName);
    }

    public static OperatorElementType operator(String operatorName) {
        return new OperatorElementType(operatorName);
    }
}
