// This is a generated file. Not intended for manual editing.
package org.logtalk.intellij.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.logtalk.intellij.psi.LogtalkTypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import org.logtalk.intellij.psi.*;

public class LogtalkBasicTermImpl extends ASTWrapperPsiElement implements LogtalkBasicTerm {

  public LogtalkBasicTermImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LogtalkVisitor visitor) {
    visitor.visitBasicTerm(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LogtalkVisitor) accept((LogtalkVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public LogtalkAtom getAtom() {
    return findChildByClass(LogtalkAtom.class);
  }

  @Override
  @Nullable
  public LogtalkBracedBlock getBracedBlock() {
    return findChildByClass(LogtalkBracedBlock.class);
  }

  @Override
  @Nullable
  public LogtalkCompound getCompound() {
    return findChildByClass(LogtalkCompound.class);
  }

  @Override
  @Nullable
  public LogtalkList getList() {
    return findChildByClass(LogtalkList.class);
  }

  @Override
  @Nullable
  public LogtalkMapReference getMapReference() {
    return findChildByClass(LogtalkMapReference.class);
  }

  @Override
  @Nullable
  public LogtalkNumber getNumber() {
    return findChildByClass(LogtalkNumber.class);
  }

  @Override
  @Nullable
  public LogtalkParenthesizedBlock getParenthesizedBlock() {
    return findChildByClass(LogtalkParenthesizedBlock.class);
  }

  @Override
  @Nullable
  public LogtalkVariable getVariable() {
    return findChildByClass(LogtalkVariable.class);
  }

}
