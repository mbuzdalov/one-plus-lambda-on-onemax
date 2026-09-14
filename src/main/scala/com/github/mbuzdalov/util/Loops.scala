package com.github.mbuzdalov.util

object Loops:
  /**
   * A forever loop that returns `Nothing`.
   * @param body the body to execute forever.
   * @return nothing.
   */
  inline def loopForever(inline body: => Unit): Nothing =
    while true do body
    throw new AssertionError("Should never reach there")
  
  /**
   * This loops from an int `from`, inclusively,
   * until an int `until`, exclusively, and executes the specified body for each of the values.
   *
   * This is an inline function which inlines the body, so accessing local `var`s does not incur a runtime penalty.
   *
   * @param from the initial value (inclusive).
   * @param until the final value (exclusive).
   * @param body the loop body to execute.
   */
  inline def loopFromUntil(from: Int, until: Int)(inline body: Int => Unit): Unit =
    var i = from
    while i < until do
      body(i)
      i += 1
  
  /**
   * This loops from an int `from` down to an int `downTo`, both inclusively,
   * and executes the specified body for each of the values.
   *
   * This is an inline function which inlines the body, so accessing local `var`s does not incur a runtime penalty.
   * No bound checking is performed; `downTo` equal to `Int.MinValue` would loop forever.
   *
   * @param from the initial value (inclusive).
   * @param downTo the final value (inclusive).
   * @param body the loop body to execute.
   */
  inline def loopFromDownTo(from: Int, downTo: Int)(inline body: Int => Unit): Unit =
    var i = from
    while i >= downTo do
      body(i)
      i -= 1
  
  /**
   * This loops from an int `from` to an int `to`, both inclusively,
   * and executes the specified body for each of the values.
   *
   * This is an inline function which inlines the body, so accessing local `var`s does not incur a runtime penalty.
   * No bound checking is performed; `to` equal to `Int.MaxValue` would loop forever.
   *
   * @param from the initial value (inclusive).
   * @param to the final value (inclusive).
   * @param body the loop body to execute.
   */
  inline def loopFromTo(from: Int, to: Int)(inline body: Int => Unit): Unit =
    var i = from
    while i <= to do
      body(i)
      i += 1
  
  
  /**
   * Returns `true` if and only if the given predicate `body` holds for all integer arguments
   * starting with `from` (inclusive) and ending with `until` (exclusive).
   * 
   * @param from the minimum index to test (inclusive)
   * @param until the maximum index to test (exclusive)
   * @param body the predicate to test
   * @return `true` if the predicate holds for all integers in the range, `false` otherwise.
   */
  inline def forAllFromUntil(from: Int, until: Int)(inline body: Int => Boolean): Boolean =
    var i = from
    var result = true
    while i < until && result do
      result = body(i)
      i += 1
    result  