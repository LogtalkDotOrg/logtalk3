// This is a generated file. Not intended for manual editing.
package org.logtalk.intellij.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface LogtalkNativeBinaryOperation extends PsiElement {

  @NotNull
  LogtalkBasicTerm getBasicTerm();

  @NotNull
  LogtalkKnownBinaryOperator getKnownBinaryOperator();

  @NotNull
  LogtalkTerm getTerm();

}
