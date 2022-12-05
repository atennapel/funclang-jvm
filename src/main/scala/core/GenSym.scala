package core

object GenSym:
  private var gensym: Int = 0
  def uniq: Int =
    val t = gensym
    gensym += 1
    t
