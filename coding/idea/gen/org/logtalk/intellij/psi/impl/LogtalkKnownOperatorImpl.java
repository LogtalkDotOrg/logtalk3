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

public class LogtalkKnownOperatorImpl extends ASTWrapperPsiElement implements LogtalkKnownOperator {

  public LogtalkKnownOperatorImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LogtalkVisitor visitor) {
    visitor.visitKnownOperator(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LogtalkVisitor) accept((LogtalkVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public LogtalkKnownBinaryOperator getKnownBinaryOperator() {
    return findChildByClass(LogtalkKnownBinaryOperator.class);
  }

  @Override
  @Nullable
  public LogtalkKnownLeftOperator getKnownLeftOperator() {
    return findChildByClass(LogtalkKnownLeftOperator.class);
  }

}
