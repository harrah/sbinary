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
    def writes(ts : S)(out : Output) = { out.write(ts.size); out.writeAll(ts); }
  }

  /**
   * Length encodes, but with the result built from an array. 
   */
  def viaArray[S <: Collection[T], T] (f : Array[T] => S) (implicit binary : Binary[T]) : Binary[S] = new Binary[S] {
    def writes(xs : S)(out : Output) = { out.write(xs.size); out.writeAll(xs); }
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

  <#list 1..9 as i> 
  <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>
  /**
   * Represents this type as ${i} consecutive binary blocks of type T1..T${i},
   * relative to the specified way of decomposing and composing S as such.
   *  
   * This still needs some work. It's a bit painful to use.
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

<#list 2..9 as i>
  /**
   * Uses a single tag byte to represent S as a union of ${i} subtypes. The specified
   * operation is a 'fold', used to build a function over S from a list of functions
   * over the Ti. 
   */
  def asUnion${i}[S, <#list 1..i as j>T${j} <: S<#if i !=j>,</#if></#list>](
    <#list 1..i as j>
      clazz${j} : Class[T${j}]<#if i!=j>,<#else>)(implicit </#if>       
    </#list>
    <#list 1..i as j>
      bin${j} : Binary[T${j}] <#if i!=j>,<#else>) : Binary[S] = new Binary[S]{</#if>
    </#list>
      def reads (in : Input) = 
        in.read[Byte] match {
          <#list 1..i as j>
            case ${j} => in.read[T${j}]
          </#list>
        }

      def writes (s : S)(out : Output) = 
          <#list 1..i as j>
            if (clazz${j}.isInstance(s)){
              out.write[Byte](${j});
              out.write[T${j}](s.asInstanceOf[T${j}]);        
            } else <#if i==j>error("Unrecognised object: " + s);</#if>
          </#list>
    } 
</#list>

}
