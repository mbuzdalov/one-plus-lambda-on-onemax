package com.github.mbuzdalov.oll

class CommandLineArgs(args: Array[String]) {
  def getStringOption(name: String): Option[String] = {
    val prefix = "--" + name + "="
    args.find(_.startsWith(prefix)).map(_.substring(prefix.length))
  }

  def getString(name: String, suffix: => String): String = getStringOption(name) match {
    case None => throw new IllegalArgumentException(s"--$name: option not found$suffix")
    case Some(v) => v
  }

  def getBoolean(name: String): Boolean = getStringOption(name) match {
    case None => throw new IllegalArgumentException(s"--$name: option not found (expected a Boolean value)")
    case Some(v) => v.toBooleanOption match {
      case None => throw new IllegalArgumentException(s"--$name: expected a Boolean value, found '$v''")
      case Some(result) => result
    }
  }

  def getLong(name: String): Long = getStringOption(name) match {
    case None => throw new IllegalArgumentException(s"--$name: option not found (expected a 64-bit integer)")
    case Some(v) => v.toLongOption match {
      case None => throw new IllegalArgumentException(s"--$name: expected a 64-bit integer, found '$v''")
      case Some(result) => result
    }
  }
}
