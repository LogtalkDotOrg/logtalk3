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

public class LogtalkCompoundImpl extends ASTWrapperPsiElement implements LogtalkCompound {

  public LogtalkCompoundImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull LogtalkVisitor visitor) {
    visitor.visitCompound(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof LogtalkVisitor) accept((LogtalkVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public LogtalkCompoundName getCompoundName() {
    return findNotNullChildByClass(LogtalkCompoundName.class);
  }

  @Override
  @NotNull
  public LogtalkTerm getTerm() {
    return findNotNullChildByClass(LogtalkTerm.class);
  }

}
