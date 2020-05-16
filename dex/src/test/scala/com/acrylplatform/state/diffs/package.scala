package com.acrylplatform.state

import com.acrylplatform.common.state.diffs.ProduceError
import com.acrylplatform.db.WithState
import org.scalatest.Matchers

package object diffs extends WithState with Matchers {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
