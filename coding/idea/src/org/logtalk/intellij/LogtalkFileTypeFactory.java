package org.logtalk.intellij;


import org.jetbrains.annotations.NotNull;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;

public class LogtalkFileTypeFactory extends FileTypeFactory {

    @Override
    public void createFileTypes(@NotNull FileTypeConsumer fileTypeConsumer) {
        fileTypeConsumer.consume(LogtalkFileType.INSTANCE, "logtalk");
        fileTypeConsumer.consume(LogtalkFileType.INSTANCE, "lgt");
        fileTypeConsumer.consume(PrologFileType.INSTANCE, "prolog");
        fileTypeConsumer.consume(PrologFileType.INSTANCE, "pl");
    }

}
