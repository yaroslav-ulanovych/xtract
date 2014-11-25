package xtract.test.layouts

import xtract.{Obj, AbstractObj}

package cc {
  trait Figure

  case class Rectangle(width: Int, height: Int) extends Figure

  case class Circle(radius: Int) extends Figure

  case class Holder(figure: Figure)
}

package obj {
  trait Figure extends AbstractObj

  class Rectangle extends Figure {
    val width = int
    val height = int
  }

  class Circle extends Figure {
    val radius = int
  }

  class Holder extends Obj {
    val figure = embedded[Figure]
  }
}
