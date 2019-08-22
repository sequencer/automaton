package set
import uset._

object Set {
  def apply[T](x: T): Set[T] = Set(USCons(x, USNil()))
  def apply[T](): Set[T] = Set(USNil[T]())
}

object + {
  def unapply[T](set: Set[T]): Option[(T, Set[T])] = set.uset match {
    case USNil() => None
    case USCons(x, xs) => Some((x, Set(xs)))
  }
}

case class Set[T](uset: USet[T]) {
  def contains(x: T): Boolean = uset.contains(x)
  def subsetOf(that: Set[T]): Boolean = uset.subsetOf(that.uset)
  def strictSubsetOf(that: Set[T]): Boolean = uset.strictSubsetOf(that.uset)
  def same(that: Set[T]): Boolean = this.uset.same(that.uset)
  def exists(p: T => Boolean): Boolean = uset.exists(p)
  def size: BigInt = uset.size
  def ++(that: Set[T]): Set[T] = Set(this.uset ++ that.uset)
}

object SetSpecs {
  def sameRefl[T](set: Set[T]): Boolean = {
    set same set
  }
  def sameTrans[T](set1: Set[T], set2: Set[T], set3: Set[T]): Boolean = {
    require(set1.same(set2) && set2.same(set3))
    set1 same set3
  }
  def sameExists[T](set1: Set[T], set2: Set[T], p: T => Boolean): Boolean = {
    require(set1 same set2)
    set1.exists(p) == set2.exists(p)
  }
  def unionOfSubsetsIsSubset[T](set1: Set[T], set2: Set[T], set3: Set[T]): Boolean = {
    (set1.subsetOf(set3) && set2.subsetOf(set3)) == (set1 ++ set2).subsetOf(set3)
  }
  def subsetOfUnion[T](set1: Set[T], set2: Set[T]): Boolean = {
    set1.subsetOf(set1 ++ set2) && set2.subsetOf(set1 ++ set2)
  }
  def subsetIsSmallerOrEqual[T](set1: Set[T], set2: Set[T]): Boolean = {
    set1.size <= set2.size
  }
  def strictSubsetIsSmaller[T](set1: Set[T], set2: Set[T]): Boolean = {
    set1.size < set2.size
  }
}
