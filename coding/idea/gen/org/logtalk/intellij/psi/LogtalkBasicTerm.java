// This is a generated file. Not intended for manual editing.
package org.logtalk.intellij.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface LogtalkBasicTerm extends PsiElement {

  @Nullable
  LogtalkAtom getAtom();

  @Nullable
  LogtalkBracedBlock getBracedBlock();

  @Nullable
  LogtalkCompound getCompound();

  @Nullable
  LogtalkList getList();

  @Nullable
  LogtalkMapReference getMapReference();

  @Nullable
  LogtalkNumber getNumber();

  @Nullable
  LogtalkParenthesizedBlock getParenthesizedBlock();

  @Nullable
  LogtalkVariable getVariable();

}
