package de.hyronx.matter.compiler.types

import java.nio.file.Path

import de.hyronx.matter.compiler.errors.TypeError
import de.hyronx.matter.library.MutableTree

// The most nested type is on the path's end
sealed trait TypePath extends Path {
  lazy val name: String = getFileName.toString
  lazy val parents: Stream[String] = typePath.dropRight(1)
  override lazy val toString: String = typePath.mkString(".")
  override lazy val getNameCount: Int = typePath.length
  override lazy val getFileName: Path = RelativeTypePath(typePath.lastOption match {
    case Some(last) ⇒ Stream(last)
    case None       ⇒ Stream()
  })
  override lazy val getRoot: Path = RelativeTypePath(typePath.headOption match {
    case Some(head) ⇒ Stream(head)
    case None       ⇒ Stream()
  })
  override lazy val getParent: Path = RelativeTypePath(typePath.takeRight(2).dropRight(1))

  def toType[T <: MutableTree[TypeTrait]](
    parent: Option[T] = None
  ): TypeTrait = {
    getType().getOrElse(throw TypeError(s"Type ${typePath.mkString(".")} not found"))
  }

  def getType[T <: MutableTree[TypeTrait]](
    parent: Option[T] = None
  ): Option[TypeTrait] = {
    Types.root
      .find(_ == this.toAbsolutePath)
      // If this is not a real absolute path, try to find `name` in `parent`'s children
      .orElse(parent.flatMap { parent ⇒
        parent.findChild(_ == name).map(_.asInstanceOf[TypeTrait])
      })
  }

  override def equals(that: Any): Boolean = that match {
    case that: TypePath ⇒ that.typePath == typePath
    case _              ⇒ false
  }

  override def startsWith(that: String): Boolean = typePath.headOption match {
    case Some(head) ⇒ head == that
    case None       ⇒ false
  }

  override def startsWith(that: Path): Boolean = that match {
    case that: TypePath ⇒ typePath.startsWith(that.typePath)
    case _              ⇒ false
  }

  override def endsWith(that: String): Boolean = typePath.lastOption match {
    case Some(last) ⇒ last == that
    case None       ⇒ false
  }

  override def endsWith(that: Path): Boolean = that match {
    case that: TypePath ⇒ typePath.endsWith(that.typePath)
    case _              ⇒ false
  }

  override def getName(index: Int): Path = RelativeTypePath(Stream(typePath(index)))

  override def compareTo(that: Path): Int = that match {
    case that: TypePath ⇒ that.typePath.length.compareTo(typePath.length)
    case _              ⇒ that.compareTo(this)
  }

  override def resolve(that: String): Path = resolve(RelativeTypePath(Stream(that)))

  override def resolve(that: Path): Path = (this, that) match {
    case (self: AbsoluteTypePath, that: RelativeTypePath) ⇒
      self.copy(_path = self.asInstanceOf[TypePath].typePath ++ that.typePath)
    case (self: RelativeTypePath, that: AbsoluteTypePath) ⇒
      that.copy(_path = that.asInstanceOf[TypePath].typePath ++ self.typePath)
    case (self: RelativeTypePath, that: RelativeTypePath) ⇒
      self.copy(typePath = self.typePath ++ that.typePath)
    case _ ⇒ throw new IllegalArgumentException()
  }

  override def resolveSibling(that: String): Path = resolveSibling(RelativeTypePath(Stream(that)))

  override def resolveSibling(that: Path): Path = (this, that) match {
    case (self: AbsoluteTypePath, that: RelativeTypePath) ⇒
      self.copy(_path = self.asInstanceOf[TypePath].typePath.dropRight(that.typePath.length) ++ that.typePath)
    case (self, that: AbsoluteTypePath) ⇒ that
    case _                              ⇒ throw new IllegalArgumentException()
  }

  override def getFileSystem(): java.nio.file.FileSystem = throw new java.lang.UnsupportedOperationException()

  override def iterator(): java.util.Iterator[java.nio.file.Path] = throw new java.lang.UnsupportedOperationException()

  override def normalize(): java.nio.file.Path = throw new java.lang.UnsupportedOperationException()

  override def register(x$1: java.nio.file.WatchService, x$2: java.nio.file.WatchEvent.Kind[_]*): java.nio.file.WatchKey = throw new java.lang.UnsupportedOperationException()

  override def register(x$1: java.nio.file.WatchService, x$2: Array[java.nio.file.WatchEvent.Kind[_]], x$3: java.nio.file.WatchEvent.Modifier*): java.nio.file.WatchKey = throw new java.lang.UnsupportedOperationException()

  override def relativize(x$1: java.nio.file.Path): java.nio.file.Path = throw new java.lang.UnsupportedOperationException()

  override def subpath(x$1: Int, x$2: Int): java.nio.file.Path = throw new java.lang.UnsupportedOperationException()

  override def toFile(): java.io.File = throw new java.lang.UnsupportedOperationException()

  override def toRealPath(x$1: java.nio.file.LinkOption*): java.nio.file.Path = throw new java.lang.UnsupportedOperationException()

  override def toUri(): java.net.URI = throw new java.lang.UnsupportedOperationException()

  private[types] val typePath: Stream[String]
}

object TypePath {
  val Root = "Base"
}

case class AbsoluteTypePath(_path: Stream[String]) extends TypePath {
  override lazy val toString: String = typePath.drop(1).mkString(".")
  override lazy val isAbsolute: Boolean = true
  override lazy val toAbsolutePath: Path = this
  override val typePath: Stream[String] = _path match {
    case head #:: _ if head == TypePath.Root ⇒ _path
    case _                                   ⇒ TypePath.Root #:: _path
  }

  private[types] def this(typePath: Stream[TypeTrait], unused1: Option[Nothing]) = this(typePath.map(_.name))
}

case class RelativeTypePath(typePath: Stream[String]) extends TypePath {
  override lazy val isAbsolute: Boolean = false
  override lazy val toAbsolutePath: Path = AbsoluteTypePath(parents :+ name)
}
