package org.logtalk.intellij;


import java.io.Reader;

import com.intellij.lexer.FlexAdapter;

public class LogtalkLexerAdapter extends FlexAdapter {

    public LogtalkLexerAdapter() {
        super(new LogtalkLexer((Reader) null));
    }

}
