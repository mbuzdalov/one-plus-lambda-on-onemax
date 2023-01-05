package com.github.mbuzdalov.oll

class CommandLineArgs(args: Array[String]) {
  def get(name: String): Option[String] = {
    val prefix = "--" + name + "="
    args.find(_.startsWith(prefix)).map(_.substring(prefix.length))
  }

  def getBoolean(name: String): Boolean = get(name) match {
    case None => throw new IllegalArgumentException(s"--$name: option not found (expected a Boolean value)")
    case Some(v) => v.toBooleanOption match {
      case None => throw new IllegalArgumentException(s"--$name: expected a Boolean value, found '$v''")
      case Some(result) => result
    }
  }

  def getLong(name: String): Long = get(name) match {
    case None => throw new IllegalArgumentException(s"--$name: option not found (expected a 64-bit integer)")
    case Some(v) => v.toLongOption match {
      case None => throw new IllegalArgumentException(s"--$name: expected a 64-bit integer, found '$v''")
      case Some(result) => result
    }
  }
}
