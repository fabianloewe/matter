package de.hyronx.matter.library

trait MutableTree[T <: MutableTree[T]] extends Cloneable { self: T ⇒
  //type CovariantTree = R forSome { type R <: MutableTree[T] }

  protected var _children: Stream[T] = Stream()
  protected var _parent: Option[T] = None

  def children: Stream[T] = _children

  def parent: Option[T] = _parent

  def parent_=(newParent: T): Unit = _parent = Some(newParent)

  def duplicate(newParent: Option[T] = self.parent): T = {
    val newCopy = self.clone().asInstanceOf[T]
    newCopy._children = newCopy.children.map(_.duplicate())
    newCopy._parent = newParent
    newCopy
  }

  def hasChildren: Boolean = children.nonEmpty

  def firstChild: Option[T] = children.headOption

  def lastChild: Option[T] = children.lastOption

  def getChild(n: Int): Option[T] = children.lift(n)

  def findChild(p: T ⇒ Boolean): Option[T] = children.find(p)

  def root: T = parent.map(_.root).getOrElse(self)

  def isRoot: Boolean = parent.isEmpty

  def isLeaf: Boolean = children.isEmpty

  /*
  private def buildPath(): Stream[CovariantTree] = parent match {
    case None => Stream(self)
    case Some(parent) => self #:: parent.buildPath()
  }
  */

  def path(): Stream[T] = parent match {
    case None         ⇒ Stream(self)
    case Some(parent) ⇒ parent.path :+ self
  }

  def addChild[R <: T](child: R): R = {
    _children = children :+ child
    child._parent = Some(self)
    child
  }

  def addChildren(children: T*): Unit = children.foreach(addChild)

  def print(
    detailed: Boolean = false,
    indent: Int = 0,
    withSiblings: Boolean = true
  ): Unit = println((" " * indent) + toString)

  def printTree(
    detailed: Boolean = false,
    indent: Int = 0,
    withSiblings: Boolean = true
  ): Unit = {
    print(detailed, indent, withSiblings)

    // MutableTree.foreach cannot be used because the indentation wouldn't work
    children foreach (_.printTree(detailed, indent + 2, withSiblings))
  }

  def replaceNode[R <: T](newNode: ⇒ R): R = {
    newNode._children = children.map { child ⇒
      child._parent = Some(newNode)
      child
    }
    newNode._parent = _parent.map { p ⇒
      p._children = p._children.filterNot(_ == self) :+ newNode
      p
    }

    self._children = Stream()
    self._parent = None

    newNode
  }

  private def iterate[R](p: T ⇒ Option[R]) = {
    val it = children.iterator
    var found: Option[R] = None
    while (found.isEmpty && it.hasNext) {
      val next = it.next()
      found = p(next.asInstanceOf[T])
    }
    found
  }

  private def findWithoutSelf(p: T ⇒ Boolean): Option[T] = iterate { next ⇒
    if (p(next)) Some(next)
    else next.findWithoutSelf(p)
  }

  def find(p: T ⇒ Boolean): Option[T] = {
    if (p(self)) Some(self)
    else findWithoutSelf(p)
  }

  def collectFirst[R](p: PartialFunction[T, R]): Option[R] = {
    if (p.isDefinedAt(self)) {
      Some(p(self))
    } else {
      iterate { next ⇒
        if (p.isDefinedAt(next)) Some(p(next))
        else next.collectFirst(p)
      }
    }
  }

  def foreach(f: T ⇒ Unit, withSelf: Boolean = false): Unit = {
    /*
    @scala.annotation.tailrec
    def eachChild(list: Stream[T]): Unit = list match {
      case Stream() ⇒
      case head #:: tail ⇒
        f(head.asInstanceOf[T])
        head foreach f
        eachChild(tail)
    }
    */

    if (withSelf)
      f(self)

    children foreach { child ⇒
      f(child)
      child.foreach(f)
    }
  }
}

object MutableTree {
  def apply[R <: MutableTree[R], T <: R](root: T, children: R*): T = {
    root._children = children.toStream
    children.foreach(_._parent = Some(root))
    root
  }

  def unapply[T <: MutableTree[T]](arg: T): Option[(T, Stream[T])] = Some((arg, arg.children))
}
