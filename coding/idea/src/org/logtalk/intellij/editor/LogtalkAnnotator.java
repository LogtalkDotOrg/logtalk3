package org.logtalk.intellij.editor;


import static org.logtalk.intellij.psi.LogtalkElementType.getElementType;
import static org.logtalk.intellij.psi.LogtalkElementType.isAtomKeyword;
import static org.logtalk.intellij.psi.LogtalkElementType.isCompoundNameKeyword;
import static org.logtalk.intellij.psi.LogtalkElementType.isKnownBinaryOperator;
import static org.logtalk.intellij.psi.LogtalkElementType.isKnownLeftOperator;

import org.jetbrains.annotations.NotNull;

import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;

public class LogtalkAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (shouldAnnotate(element)) {
            highlightTokens(element, holder, new LogtalkSyntaxHighlighter());
        }
    }

    private static void highlightTokens(PsiElement element, AnnotationHolder holder, SyntaxHighlighter highlighter) {
        TextAttributesKey[] keys = highlighter.getTokenHighlights(getElementType(element));
        for (TextAttributesKey key : keys) {
            Annotation annotation = holder.createInfoAnnotation(element.getNode(), getMessage(element));
            TextAttributes attributes = EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key);
            annotation.setEnforcedTextAttributes(attributes);
        }
    }

    private static boolean shouldAnnotate(PsiElement element) {
        IElementType elementType = getElementType(element);
        return isKnownBinaryOperator(elementType) ||
                isKnownLeftOperator(elementType) ||
                isCompoundNameKeyword(elementType) ||
                isAtomKeyword(elementType);
    }

    private static String getMessage(PsiElement element) {
        IElementType elementType = getElementType(element);
        if (isKnownBinaryOperator(elementType)) {
            return "Binary operator";
        } else if (isKnownLeftOperator(elementType)) {
            return "Left operator";
        } else if (isCompoundNameKeyword(elementType)) {
            return "Functor keyword";
        } else if (isAtomKeyword(elementType)) {
            return "Atom keyword";
        } else {
            throw new AssertionError();
        }
    }


/*    private static void highlightTokens(final Property property, final ASTNode node, final AnnotationHolder holder, PropertiesHighlighter highlighter) {
        Lexer lexer = highlighter.getHighlightingLexer();
        final String s = node.getText();
        lexer.start(s);

        while (lexer.getTokenType() != null) {
            IElementType elementType = lexer.getTokenType();
            TextAttributesKey[] keys = highlighter.getTokenHighlights(elementType);
            for (TextAttributesKey key : keys) {
                Pair<String,HighlightSeverity> pair = PropertiesHighlighter.DISPLAY_NAMES.get(key);
                String displayName = pair.getFirst();
                HighlightSeverity severity = pair.getSecond();
                if (severity != null) {
                    int start = lexer.getTokenStart() + node.getTextRange().getStartOffset();
                    int end = lexer.getTokenEnd() + node.getTextRange().getStartOffset();
                    TextRange textRange = new TextRange(start, end);
                    final Annotation annotation;
                    if (severity == HighlightSeverity.WARNING) {
                        annotation = holder.createWarningAnnotation(textRange, displayName);
                    }
                    else if (severity == HighlightSeverity.ERROR) {
                        annotation = holder.createErrorAnnotation(textRange, displayName);
                    }
                    else {
                        annotation = holder.createInfoAnnotation(textRange, displayName);
                    }
                    TextAttributes attributes = EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key);
                    annotation.setEnforcedTextAttributes(attributes);
                    if (key == PropertiesHighlighter.PROPERTIES_INVALID_STRING_ESCAPE) {
                        annotation.registerFix(new IntentionAction() {
                            @NotNull
                            public String getText() {
                                return PropertiesBundle.message("unescape");
                            }

                            @NotNull
                            public String getFamilyName() {
                                return getText();
                            }

                            public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
                                if (!property.isValid() || !property.getManager().isInProject(property)) return false;

                                String text = property.getPropertiesFile().getContainingFile().getText();
                                int startOffset = annotation.getStartOffset();
                                return text.length() > startOffset && text.charAt(startOffset) == '\\';
                            }

                            public void invoke(@NotNull Project project, Editor editor, PsiFile file) {
                                int offset = annotation.getStartOffset();
                                if (property.getPropertiesFile().getContainingFile().getText().charAt(offset) == '\\') {
                                    editor.getDocument().deleteString(offset, offset+1);
                                }
                            }

                            public boolean startInWriteAction() {
                                return true;
                            }
                        });
                    }
                }
            }
            lexer.advance();
        }
    }*/
}
