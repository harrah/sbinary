package scalazextensions;

import scalaz.Equal._;
import scala.collection._;
import scalaz._;

object EqualityInstances{

  /**
   * Note that value equality of immutable maps is defined purely in terms
   * of value equality of the *Values*. Keys use their normal equality test.
   * This is necessary due to the semantics of Map. 
   */
  implicit def ImmutableMapEqual[K, V](implicit e : Equal[V]) : Equal[immutable.Map[K, V]] = new Equal[immutable.Map[K, V]]{
    override def eq(x : immutable.Map[K, V], y : immutable.Map[K, V]) =
      if (x.size != y.size) false;
      else x.forall((u : (K, V)) => y.get(u._1) match {
        case None => false;
        case Some(v) => e(u._2, v);
      })    
  }

// Generated instances for tuples.
<#list 2..22 as i>
  <#assign typeParam><#list 1..i as j>T${j}<#if i!=j>, </#if></#list></#assign>
  <#assign typeString>Tuple${i}[${typeParam}]</#assign>
  implicit def Tuple${i}Equal[${typeParam}](implicit 
    <#list 1..i as j>
      eq${j} : Equal[T${j}]<#if i != j>,<#else>) =</#if>
    </#list>    
    new Equal[${typeString}](){
      override def eq(x : ${typeString}, y : ${typeString}) = 
<#list 1..i as j>
        eq${j}(x._${j}, y._${j})<#if i!=j>&&<#else>;</#if>
</#list>          
    }

</#list>
}
