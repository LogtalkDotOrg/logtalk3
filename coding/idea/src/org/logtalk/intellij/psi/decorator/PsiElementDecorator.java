package org.logtalk.intellij.psi.decorator;


import javax.swing.*;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiInvalidElementAccessException;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.util.IncorrectOperationException;

public  class PsiElementDecorator implements PsiElement {

    private final PsiElement decoratedPsiElement;

    protected PsiElementDecorator(PsiElement psiElement) {
        this.decoratedPsiElement = psiElement;
    }

    public PsiElement getDecoratedPsiElement() {
        return decoratedPsiElement;
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public Project getProject() throws PsiInvalidElementAccessException {
        return decoratedPsiElement.getProject();
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public Language getLanguage() {
        return decoratedPsiElement.getLanguage();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiManager getManager() {
        return decoratedPsiElement.getManager();
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public PsiElement[] getChildren() {
        return decoratedPsiElement.getChildren();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getParent() {
        return decoratedPsiElement.getParent();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getFirstChild() {
        return decoratedPsiElement.getFirstChild();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getLastChild() {
        return decoratedPsiElement.getLastChild();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getNextSibling() {
        return decoratedPsiElement.getNextSibling();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getPrevSibling() {
        return decoratedPsiElement.getPrevSibling();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiFile getContainingFile() throws PsiInvalidElementAccessException {
        return decoratedPsiElement.getContainingFile();
    }

    @Override
    @Contract(
        pure = true
    )
    public TextRange getTextRange() {
        return decoratedPsiElement.getTextRange();
    }

    @Override
    @Contract(
        pure = true
    )
    public int getStartOffsetInParent() {
        return decoratedPsiElement.getStartOffsetInParent();
    }

    @Override
    @Contract(
        pure = true
    )
    public int getTextLength() {
        return decoratedPsiElement.getTextLength();
    }

    @Override
    @Nullable
    @Contract(
        pure = true
    )
    public PsiElement findElementAt(final int offset) {
        return decoratedPsiElement.findElementAt(offset);
    }

    @Override
    @Nullable
    @Contract(
        pure = true
    )
    public PsiReference findReferenceAt(final int offset) {
        return decoratedPsiElement.findReferenceAt(offset);
    }

    @Override
    @Contract(
        pure = true
    )
    public int getTextOffset() {
        return decoratedPsiElement.getTextOffset();
    }

    @Override
    @NonNls
    @Contract(
        pure = true
    )
    public String getText() {
        return decoratedPsiElement.getText();
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public char[] textToCharArray() {
        return decoratedPsiElement.textToCharArray();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getNavigationElement() {
        return decoratedPsiElement.getNavigationElement();
    }

    @Override
    @Contract(
        pure = true
    )
    public PsiElement getOriginalElement() {
        return decoratedPsiElement.getOriginalElement();
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean textMatches(@NotNull @NonNls final CharSequence text) {
        return decoratedPsiElement.textMatches(text);
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean textMatches(@NotNull final PsiElement element) {
        return decoratedPsiElement.textMatches(element);
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean textContains(final char c) {
        return decoratedPsiElement.textContains(c);
    }

    @Override
    public void accept(@NotNull final PsiElementVisitor visitor) {
        decoratedPsiElement.accept(visitor);
    }

    @Override
    public void acceptChildren(@NotNull final PsiElementVisitor visitor) {
        decoratedPsiElement.acceptChildren(visitor);
    }

    @Override
    public PsiElement copy() {
        return decoratedPsiElement.copy();
    }

    @Override
    public PsiElement add(@NotNull final PsiElement element) throws IncorrectOperationException {
        return decoratedPsiElement.add(element);
    }

    @Override
    public PsiElement addBefore(@NotNull final PsiElement element, @Nullable final PsiElement anchor) throws IncorrectOperationException {
        return decoratedPsiElement.addBefore(element, anchor);
    }

    @Override
    public PsiElement addAfter(@NotNull final PsiElement element, @Nullable final PsiElement anchor) throws IncorrectOperationException {
        return decoratedPsiElement.addAfter(element, anchor);
    }

    @Override
    public void checkAdd(@NotNull final PsiElement element) throws IncorrectOperationException {
        decoratedPsiElement.checkAdd(element);
    }

    @Override
    public PsiElement addRange(final PsiElement first, final PsiElement last) throws IncorrectOperationException {
        return decoratedPsiElement.addRange(first, last);
    }

    @Override
    public PsiElement addRangeBefore(@NotNull final PsiElement first, @NotNull final PsiElement last, final PsiElement anchor) throws IncorrectOperationException {
        return decoratedPsiElement.addRangeBefore(first, last, anchor);
    }

    @Override
    public PsiElement addRangeAfter(final PsiElement first, final PsiElement last, final PsiElement anchor) throws IncorrectOperationException {
        return decoratedPsiElement.addRangeAfter(first, last, anchor);
    }

    @Override
    public void delete() throws IncorrectOperationException {
        decoratedPsiElement.delete();
    }

    @Override
    public void checkDelete() throws IncorrectOperationException {
        decoratedPsiElement.checkDelete();
    }

    @Override
    public void deleteChildRange(final PsiElement first, final PsiElement last) throws IncorrectOperationException {
        decoratedPsiElement.deleteChildRange(first, last);
    }

    @Override
    public PsiElement replace(@NotNull final PsiElement newElement) throws IncorrectOperationException {
        return decoratedPsiElement.replace(newElement);
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean isValid() {
        return decoratedPsiElement.isValid();
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean isWritable() {
        return decoratedPsiElement.isWritable();
    }

    @Override
    @Nullable
    @Contract(
        pure = true
    )
    public PsiReference getReference() {
        return decoratedPsiElement.getReference();
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public PsiReference[] getReferences() {
        return decoratedPsiElement.getReferences();
    }

    @Override
    @Nullable
    @Contract(
        pure = true
    )
    public <T> T getCopyableUserData(final Key<T> key) {
        return decoratedPsiElement.getCopyableUserData(key);
    }

    @Override
    public <T> void putCopyableUserData(final Key<T> key, @Nullable final T value) {
        decoratedPsiElement.putCopyableUserData(key, value);
    }

    @Override
    public boolean processDeclarations(@NotNull final PsiScopeProcessor processor, @NotNull final ResolveState state, @Nullable final PsiElement lastParent, @NotNull final PsiElement place) {
        return decoratedPsiElement.processDeclarations(processor, state, lastParent, place);
    }

    @Override
    @Nullable
    @Contract(
        pure = true
    )
    public PsiElement getContext() {
        return decoratedPsiElement.getContext();
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean isPhysical() {
        return decoratedPsiElement.isPhysical();
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public GlobalSearchScope getResolveScope() {
        return decoratedPsiElement.getResolveScope();
    }

    @Override
    @NotNull
    @Contract(
        pure = true
    )
    public SearchScope getUseScope() {
        return decoratedPsiElement.getUseScope();
    }

    @Override
    @Contract(
        pure = true
    )
    public ASTNode getNode() {
        return decoratedPsiElement.getNode();
    }

    @Override
    @NonNls
    @Contract(
        pure = true
    )
    public String toString() {
        return decoratedPsiElement.toString();
    }

    @Override
    @Contract(
        pure = true
    )
    public boolean isEquivalentTo(final PsiElement another) {
        return decoratedPsiElement.isEquivalentTo(another);
    }

    @Override
    @Nullable
    public <T> T getUserData(@NotNull final Key<T> key) {
        return decoratedPsiElement.getUserData(key);
    }

    @Override
    public <T> void putUserData(@NotNull final Key<T> key, @Nullable final T value) {
        decoratedPsiElement.putUserData(key, value);
    }

    @Override
    public Icon getIcon(@IconFlags final int flags) {
        return decoratedPsiElement.getIcon(flags);
    }
}
