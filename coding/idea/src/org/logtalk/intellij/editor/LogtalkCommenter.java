package org.logtalk.intellij.editor;

import org.jetbrains.annotations.Nullable;

import com.intellij.lang.Commenter;


public class LogtalkCommenter implements Commenter {


    @Nullable
    @Override
    public String getLineCommentPrefix() {
        return "%";
    }

    @Nullable
    @Override
    public String getBlockCommentPrefix() {
        return "/*";
    }

    @Nullable
    @Override
    public String getBlockCommentSuffix() {
        return "*/";
    }

    @Nullable
    @Override
    public String getCommentedBlockCommentPrefix() {
        return "/**";
    }

    @Nullable
    @Override
    public String getCommentedBlockCommentSuffix() {
        return "*/";
    }
}
