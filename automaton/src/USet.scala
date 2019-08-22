package uset

sealed abstract class USet[T] {
  def contains(x: T): Boolean = this match {
    case USNil() => false
    case USCons(y, ys) => (x == y) || ys.contains(x)
  }
  def subsetOf(that: USet[T]): Boolean = this match {
    case USNil() => true
    case USCons(x, xs) => that.contains(x) && xs.subsetOf(that)
  }
  def strictSubsetOf(that: USet[T]): Boolean =
    subsetOf(that) && !that.subsetOf(this)
  def same(that: USet[T]): Boolean = this.subsetOf(that) && that.subsetOf(this)
  def exists(p: T => Boolean): Boolean = this match {
    case USNil() => false
    case USCons(x, xs) => p(x) || xs.exists(p)
  }
  def find(p: T => Boolean): T = {
    this match {
      case USCons(x, xs) => if (p(x)) x else xs.find(p)
    }
  }
  def size: BigInt = {
    this match {
      case USNil() => BigInt(0)
      case USCons(_, xs) => 1 + xs.size
    }
  }
  def +(y: T): USet[T] = {
    this match {
      case USNil() =>
        USCons(y, USNil())
      case USCons(x, xs) =>
        if (x == y) {
          this
        } else {
          USCons(x, xs + y)
        }
    }
  }
  def ++(that: USet[T]): USet[T] = {
    this match {
      case USNil() => that
      case USCons(x, xs) =>
        (xs ++ that) + x
    }
  }
  def -(y: T): USet[T] = {
    this match {
      case USNil() => USNil()
      case USCons(x, xs) =>
        if (x == y) xs
        else USCons(x, xs - y)
    }
  }
  def --(that: USet[T]): USet[T] = {
    that match {
      case USNil() => this
      case USCons(y, ys) =>
        (this -- ys) - y
    }
  }
}

case class USCons[T](x: T, xs: USet[T]) extends USet[T]
case class USNil[T]() extends USet[T]

object USetSpecs {
  def setInvariant[T](set: USet[T]): Boolean = set match {
    case USNil() => true
    case USCons(x, xs) => !xs.contains(x) && setInvariant(xs)
  }
  def containsDistAdd[T](set: USet[T], y: T, z: T): Boolean = {
    (set + y).contains(z) == (set.contains(z) || y == z)
  }
  def addIsSound[T](set: USet[T], y: T): Boolean = {
    setInvariant(set + y)
  }
  def addExists[T](set: USet[T], x: T, p: T => Boolean): Boolean = {
    (set + x).exists(p) == (p(x) || set.exists(p))
  }
  def addId[T](set: USet[T], x: T): Boolean = {
    (set + x) == set
  }
  def subContains1[T](set: USet[T], y: T, z: T): Boolean = {
    (set - y).contains(z) == set.contains(z)
  }
  def subIsSound[T](set: USet[T], y: T): Boolean = {
    setInvariant(set - y)
  }
  def subsetRefl[T](set: USet[T]): Boolean = {
    set subsetOf set
  }
  def subsetTail[T](set: USet[T]): Boolean = {
    set match {
      case USNil() => true
      case USCons(x, xs) => xs.subsetOf(set)
    }
  }
  def subsetCons[T](s1: USet[T], s2: USet[T], x: T): Boolean = {
    s1.subsetOf(USCons(x, s2))
  }
  def subsetTrans[T](set1: USet[T], set2: USet[T], set3: USet[T]): Boolean = {
    set1 subsetOf set3
  }
  def containsTrans[T](set1: USet[T], set2: USet[T], x: T): Boolean = {
    set2.contains(x)
  }
  def subsetExists[T](set1: USet[T], set2: USet[T], p: T => Boolean): Boolean = {
    set2.exists(p)
  }
  def sameRefl[T](set: USet[T]): Boolean = {
    set same set
  }
  def sameTrans[T](set1: USet[T], set2: USet[T], set3: USet[T]): Boolean = {
    set1 same set3
  }
  def sameExists[T](set1: USet[T], set2: USet[T], p: T => Boolean): Boolean = {
    set1.exists(p) == set2.exists(p)
  }
  def subsetOfUnion[T](set1: USet[T], set2: USet[T]): Boolean = {
    set1.subsetOf(set1 ++ set2) && set2.subsetOf(set1 ++ set2)
  }
  def unionOfSubsetsIsSubset[T](set1: USet[T], set2: USet[T], set3: USet[T]): Boolean = {
    (set1.subsetOf(set3) && set2.subsetOf(set3)) == (set1 ++ set2).subsetOf(set3)
  }
  def subsetAdd[T](set1: USet[T], set2: USet[T], x: T): Boolean = {
    (set1 + x).subsetOf(set2) == (set1.subsetOf(set2) && set2.contains(x))
  }
  def diffContains[T](set1: USet[T], set2: USet[T], z: T): Boolean = {
    (set1 -- set2).contains(z)
  }
  def subDecSize[T](set: USet[T], y: T): Boolean = {
    set.contains(y) == ((set - y).size == set.size - 1)
  }
  def diffSubsetSize[T](set1: USet[T], set2: USet[T]): Boolean = {
    (set1 -- set2).size == set1.size - set2.size
  }
  def subsetIsSmallerOrEqual[T](set1: USet[T], set2: USet[T]): Boolean = {
    set1.size <= set2.size
  }
  def strictSubsetIsSmaller[T](s1: USet[T], s2: USet[T]): Boolean = {
    s1.size > s2.size
  }
  def notSubsetContains[T](s1: USet[T], s2: USet[T]): Boolean = {
    s1.exists(x => !s2.contains(x))
  }
}
