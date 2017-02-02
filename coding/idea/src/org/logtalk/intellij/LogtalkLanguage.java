package org.logtalk.intellij;


import com.intellij.lang.Language;

public class LogtalkLanguage extends Language {

    public static final LogtalkLanguage INSTANCE = new LogtalkLanguage();

    private LogtalkLanguage() {
        super("Logtalk");
    }
}
