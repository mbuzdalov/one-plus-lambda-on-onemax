package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.TransitionMatrix

trait TransitionMatrixFactory {
  def create(n: Int, distance: Int): TransitionMatrix
}
