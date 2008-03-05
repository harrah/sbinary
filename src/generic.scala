/**
 * Generic operations for building binary instances.
 */
package sbinary.generic;
import sbinary.Operations._;
import scala.collection.mutable.{ListBuffer, ArrayBuffer};
import scala.collection._;

import java.io._;

/**
 * Defines a type class for generic building of collections.
 */
object Building{
  trait Buildable[I[_]]{
    def builder[T] () : Builder[T]
    def builderOfCapacity[T](n : Int) : Builder[T] = builder[T] ();

    trait Builder[T]{
      def += (t : T) : Unit;
      def ++= (ts : T*) = for (t <- ts) { this += t }
      def build : I[T];
    }
  }

  implicit object ListIsBuildable extends Buildable[List]{
    def builder[T] () = new Builder[T]{
      val buffer = new ListBuffer[T]();
      def += (t : T) = buffer += t;
      def build = buffer.toList;
    } 
  }

  implicit object ImmutableSetIsBuildable extends Buildable[immutable.Set]{
    def builder[T] () = new Builder[T]{
      val buffer = new ListBuffer[T]();
      def += (t : T) = buffer += t;
      def build = immutable.Set(buffer.toList :_*);
    } 
  }

  implicit object ArrayIsBuildable extends Buildable[Array]{
    class ArrayBuilder[T](buffer : ArrayBuffer[T]) extends Builder[T]{
      def += (t : T) = (buffer += t);
      def build = buffer.toArray;     
    }

    def builder[T] () = new ArrayBuilder[T](new ArrayBuffer[T])
    override def builderOfCapacity[T](n : Int) = new ArrayBuilder[T](new ArrayBuffer[T]{
      ensureSize(n);
    });
  }
}

/**
 * Generic operations for creating binary instances
 */
object Generic {
  import Building._;
  import Instances._;

  /** 
   * Binary instance which encodes the collection by first writing the length
   * of the collection as an int, then writing the collection elements in order.
   */
  def lengthEncoded[S, T[R] <: Collection[R]](implicit bin : Binary[S], build : Buildable[T]) = new Binary[T[S]]{
    def reads(in : Input) : T[S] = {
      val length = in.read[Int];
      readMany[S, T](length)(in);
    }

    def writes(ts : T[S])(out : Output) = {
      out.write(ts.size);
      ts.foreach(out.write[S](_ : S));
    }
  }

  /** 
   * Read n elements of the specified type as the desired result type
   */
  def readMany[S, I[_]](length : Int)(in : Input)(implicit bin : Binary[S], build : Buildable[I]) : I[S] = {
      val buffer = build.builderOfCapacity[S](length);
      for (i <-  0 until length){
        buffer += in.read[S];
      }
      buffer.build;
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

  def lazyBinary[S](bin : =>Binary[S]) = new Binary[S]{
    lazy val delegate = bin;

    def reads(in : Input) = delegate.reads(in);
    def writes(s : S)(out : Output) = delegate.writes(s)(out);
  }

  <#list 1..9 as i> 
  <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>
  /**
   *  Represents this type as ${i} consecutive binary blocks of type T1..T${i},
   *  relative to the specified way of decomposing and composing S as such.
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
   * Uses a single tag bit to represent S as a union of ${i} subtypes. The specified
   * operation is a 'fold', used to build a function over S from a list of functions
   * over the Ti. 
   */
  def asUnion${i}[S, <#list 1..i as j>T${j} <: S<#if i !=j>,</#if></#list>](fold : (
    <#list 1..i as j>
      T${j} => Unit <#if i!=j>,<#else>) => (S => Unit)</#if>       
    </#list>
  ) (implicit 
    <#list 1..i as j>
      bin${j} : Binary[T${j}] <#if i!=j>,</#if>       
    </#list>
  ) : Binary[S] = new Binary[S]{
      def reads (in : Input) = 
        in.read[Byte] match {
          <#list 1..i as j>
            case ${j} => in.read[T${j}]
          </#list>
        }

      def writes (s : S)(out : Output) = 
        fold(
          <#list 1..i as j>
            (x : T${j}) => { out.write[Byte](${j}); out.write[T${j}](x) }<#if i!=j>,</#if>
          </#list>
        )(s)
    } 
</#list>

}
