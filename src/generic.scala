/**
 * Generic operations for building binary instances.
 */
package sbinary.generic;
import sbinary.Operations._;
import scala.collection.mutable.{ListBuffer, ArrayBuffer};
import scala.collection._;

import java.io._;

/**
 * Generic operations for creating binary instances
 */
object Generic {
  import Instances._;
  /** 
   * Binary instance which encodes the collection by first writing the length
   * of the collection as an int, then writing the collection elements in order.
   */
  abstract class LengthEncoded[S <: Collection[T], T](implicit binT : Binary[T]) extends Binary[S]{
    def build(size : Int, ts : Iterator[T]) : S;

    def reads(in : Input) = { val size = in.read[Int]; build(size, in.asIterator[T].take(size)) }
    def writes(ts : S)(out : Output) = { out.write(ts.size); ts.foreach(out.write); }
  }

  /**
   * Length encodes, but with the result built from an array. 
   */
  def viaArray[S <: Collection[T], T] (f : Array[T] => S) (implicit binary : Binary[T]) : Binary[S] = new Binary[S] {
    def writes(xs : S)(out : Output) = { out.write(xs.size); xs.foreach(out.write); }
    def reads(in : Input) = f(in.read[Array[T]]);
  }

  /**
   * Encodes and decodes via some String representation.
   */
  def viaString[T](f : String => T) = new Binary[T]{
    def reads(in : Input) = f(in.read[String]);
    def writes(t : T)(out : Output) = out.write(t.toString);
  }
  
  /**
   * Trivial serialization. Writing is a no-op, reading always returns this instance.
   */
  def asSingleton[T](t : T) : Binary[T] = new Binary[T]{
    def reads(in : Input) = t
    def writes(t : T)(out : Output) = ();
  }

  /**
   * Serializes this via a bijection to some other type. 
   */
  def wrap[S, T](to : S => T, from : T => S)(implicit bin : Binary[T]) = new Binary[S]{
    def reads(in : Input) = from(in.read[T]);
    def writes(s : S)(out : Output) = out.write(to(s));
  }

  /**
   * Lazy wrapper around a binary. Useful when you want e.g. mutually recursive binary instances.
   */
  def lazyBinary[S](bin : =>Binary[S]) = new Binary[S]{
    lazy val delegate = bin;

    def reads(in : Input) = delegate.reads(in);
    def writes(s : S)(out : Output) = delegate.writes(s)(out);
  }

  /**
   * Attaches a stamp to the data. This stamp is placed at the beginning of the format and may be used
   * to verify the integrity of the data (e.g. a magic number for the data format version). 
   */
  def withStamp[S, T](stamp : S)(binary : Binary[T])(implicit binS : Binary[S]) : Binary[T] = new Binary[T]{
    def reads(in : Input) = {
      val datastamp = in.read[S];
      if (stamp != datastamp) error("Incorrect stamp. Expected: " + stamp + ", Found: " + datastamp);
      binary.reads(in);
    }

    def writes(t : T)(out : Output) = {
      out.write(stamp);
      binary.writes(t)(out);
    }
  }

  <#list 2..9 as i> 
  <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>
  /**
   * Represents this type as ${i} consecutive binary blocks of type T1..T${i},
   * relative to the specified way of decomposing and composing S as such.
   */
  def asProduct${i}[S, ${typeParams}](apply : (${typeParams}) => S)(unapply : S => Product${i}[${typeParams}])(implicit
   <#list 1..i as j>
      bin${j} : Binary[T${j}] <#if i != j>,</#if>
    </#list>) = new Binary[S]{
       def reads (in : Input) : S = apply(
      <#list 1..i as j>
         in.read[T${j}]<#if i != j>,</#if>
      </#list>
      )

      def writes(s : S)(out : Output) = {
        val product = unapply(s);
        <#list 1..i as j>
          out.write(product._${j});
        </#list>;       
      }
    }  
</#list>

  case class Summand[T](clazz : Class[T], format : Binary[T]);
  implicit def classToSummand[T](clazz : Class[T])(implicit bin : Binary[T]) = Summand(clazz, bin);

  /**
   * Uses a single tag byte to represent S as a union of subtypes. 
   */
  def asUnion[S](summands : Summand[_ <: S]*) : Binary[S] = 
    if (summands.length >= 256) error("Sums of 256 or more elements currently not supported");
    else
    new Binary[S]{
      val mappings = summands.toArray.zipWithIndex;

      def reads (in : Input) : S = in.read(summands(in.read[Byte]).format)

      def writes (s : S)(out : Output) {
        for ((sum, i) <- mappings){
          if (sum.clazz.isInstance(s)) {
            out.write(i.toByte);
            out.write(sum.clazz.cast(s))(sum.format);
            return;
          }
        }
        error("No known sum type for object " + s);
      }
    } 
}
