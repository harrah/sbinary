/**
 * Generic operations for building binary instances.
 */
package sbinary;

import scala.collection.mutable.{ListBuffer, ArrayBuffer};
import scala.collection._;

import java.io._;
import Operations._;

trait Generic extends CoreProtocol{
  implicit def arrayFormat[T](implicit fmt : Format[T], mf: scala.reflect.Manifest[T]) : Format[Array[T]];
  // Needed to implement viaSeq
  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]];

  /** A more general LengthEncoded (arrays are no longer collections) */
  abstract class CollectionFormat[S, T](implicit binT : Format[T]) extends Format[S]{
    def size(s: S): Int
    def foreach(s: S)(f: T => Unit): Unit
    def build(size : Int, ts : Iterator[T]) : S;

    def reads(in : Input) = { val size = read[Int](in); build(size, (0 until size).map(i => read[T](in)).iterator) }
    def writes(out : Output, ts : S) = { write(out, size(ts)); foreach(ts)(write(out, _)); }
  }
  /** 
   * Format instance which encodes the collection by first writing the length
   * of the collection as an int, then writing the collection elements in order.
   */
  abstract class LengthEncoded[S <: Traversable[T], T](implicit binT : Format[T]) extends CollectionFormat[S, T]{
    def size(s: S) = s.size
    def foreach(s: S)(f: T => Unit) = s.foreach(f)
  }

  /**
   * Length encodes, but with the result built from an array.
   */
  def viaArray[S <: Traversable[T], T] (f : Array[T] => S) (implicit binary : Format[T], mf: scala.reflect.Manifest[T]) : Format[S] = new Format[S] {
    def writes(out : Output, xs : S) = { write(out, xs.size); xs.foreach(write(out, _)); }
    def reads(in : Input) = f(read[Array[T]](in));
  }
  /**
   * Length encodes, but with the result built from a Seq.
	* Useful for when a `ClassManifest` is not available the underlying type `T`.
   */
  def viaSeq[S <: Traversable[T], T] (f : Seq[T] => S) (implicit binary : Format[T]) : Format[S] = new Format[S] {
    def writes(out : Output, xs : S) = { write(out, xs.size); xs.foreach(write(out, _)); }
    def reads(in : Input) = f(read[List[T]](in));
  }

  /**
   * Encodes and decodes via some String representation.
   */
  def viaString[T](f : String => T) = new Format[T]{
    def reads(in : Input) = f(read[String](in));
    def writes(out : Output, t : T) = write(out, t.toString);
  }
  
  /**
   * Trivial serialization. Writing is a no-op, reading always returns this instance.
   */
  def asSingleton[T](t : T) : Format[T] = new Format[T]{
    def reads(in : Input) = t
    def writes(out : Output, t : T) = ();
  }

  /**
   * Serializes this via a bijection to some other type. 
   */
  def wrap[S, T](to : S => T, from : T => S)(implicit bin : Format[T]) = new Format[S]{
    def reads(in : Input) = from(read[T](in));
    def writes(out : Output, s : S) = write(out, to(s));
  }

  /**
   * Lazy wrapper around a binary. Useful when you want e.g. mutually recursive binary instances.
   */
  def lazyFormat[S](bin : =>Format[S]) = new Format[S]{
    lazy val delegate = bin;

    def reads(in : Input) = delegate.reads(in);
    def writes(out : Output, s : S) = delegate.writes(out, s);
  }

  /**
   * Attaches a stamp to the data. This stamp is placed at the beginning of the format and may be used
   * to verify the integrity of the data (e.g. a magic number for the data format version). 
   */
  def withStamp[S, T](stamp : S)(binary : Format[T])(implicit binS : Format[S]) : Format[T] = new Format[T]{
    def reads(in : Input) = {
      val datastamp = read[S](in);
      if (stamp != datastamp) error("Incorrect stamp. Expected: " + stamp + ", Found: " + datastamp);
      binary.reads(in);
    }

    def writes(out : Output, t : T) = {
      write(out, stamp);
      binary.writes(out, t);
    }
  }

  /**
   * Create a format for an enumeration, representing values by their integer IDs.
   *
   * Note that due to type system limitations we cannot enforce that you pass the right Enumeration to this method.
   * Be good.
   */
  def enumerationFormat[V <: Enumeration#Value](enumeration : Enumeration) = new Format[V]{
    def reads(in : Input) = enumeration(read[Int](in)).asInstanceOf[V]
    def writes(out : Output, value : V) = write(out, value.id)
  }

  <#list 2..9 as i> 
  <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>
  /**
   * Represents this type as ${i} consecutive binary blocks of type T1..T${i},
   * relative to the specified way of decomposing and composing S as such.
   */
  def asProduct${i}[S, ${typeParams}](apply : (${typeParams}) => S)(unapply : S => Product${i}[${typeParams}])(implicit
   <#list 1..i as j>
      bin${j} : Format[T${j}] <#if i != j>,</#if>
    </#list>) = new Format[S]{
       def reads (in : Input) : S = apply(
      <#list 1..i as j>
         read[T${j}](in)<#if i != j>,</#if>
      </#list>
      )

      def writes(out : Output, s : S) = {
        val product = unapply(s);
        <#list 1..i as j>
          write(out, product._${j});
        </#list>;       
      }
    }  
</#list>

  case class Summand[T](clazz : Class[_], format : Format[T]);
  implicit def classToSummand[T](clazz : Class[T])(implicit bin : Format[T]) : Summand[T] = Summand[T](clazz, bin);
  implicit def formatToSummand[T](format : Format[T])(implicit mf : scala.reflect.Manifest[T]) : Summand[T] = Summand[T](mf.erasure, format);
  // This is a bit gross. 
  implicit def anyToSummand[T](t : T) = Summand[T](t.asInstanceOf[AnyRef].getClass, asSingleton(t))

  /**
   * Uses a single tag byte to represent S as a union of subtypes. 
   */
  def asUnion[S](summands : Summand[_ <: S]*) : Format[S] = 
    if (summands.length >= 256) error("Sums of 256 or more elements currently not supported");
    else
    new Format[S]{
      val mappings = summands.toArray.zipWithIndex;

      def reads(in : Input) : S = read(in)(summands(read[Byte](in)).format)

      def writes(out : Output, s : S): Unit =
        mappings.find(_._1.clazz.isInstance(s)) match {
          case Some( (sum, i) ) => writeSum(out, s, sum, i)
          case None => error("No known sum type for object " + s);
        }
      private def writeSum[T](out : Output, s : S, sum : Summand[T], i : Int) {
        write(out, i.toByte);
        // 2.7/2.8 compatibility: cast added by MH
        write(out, sum.clazz.cast(s).asInstanceOf[T])(sum.format);
      }
  }
}