package com.acrylplatform.dex

package object util {
  def getSimpleName(x: Any): String = x.getClass.getSimpleName.replaceAll("\\$", "")
}
