package sbinary.equality;
import scala.collection._;

trait Eq[-T]{
  def equal(x : T, y : T) : Boolean;
}

object DefaultEq extends Eq[AnyRef]{
  def equal(x : AnyRef, y : AnyRef) = x == y; 
}

object Instances{
  def fromFunction[T](f : (T, T) => Boolean) = new Eq[T]{
    def equal(x : T, y : T) = f(x, y);
  } 
  
<#assign valueTypes = ["Byte", "Char", "Short", "Int", "Long", "Float", "Double"] />

<#list valueTypes as value>
  <#assign arrayvalue>Array[${value}]</#assign>
  implicit object ${value}IsEq extends Eq[${value}]{
    def equal(x : ${value}, y : ${value}) = x == y;
  }

  implicit object ${value}ArrayIsEq extends Eq[${arrayvalue}]{
    def equal(x : ${arrayvalue}, y : ${arrayvalue}) = x.deepEquals(y);
  }
</#list>

  implicit def arraysAreEq[T](implicit e : Eq[T]) = new Eq[Array[T]]{
    def equal(x : Array[T], y : Array[T]) = (x.length == y.length) && 
      x.zip(y).forall(u => e.equal(u._1, u._2));
  }

  implicit def listsAreEq[T](implicit e : Eq[T]) = new Eq[List[T]]{
    def equal(x : List[T], y : List[T]) = (x, y) match {
      case (Nil, Nil) => true;
      case (::(x, xs), ::(y, ys)) => if (!e.equal(x, y)) false 
                                     else equal(xs, ys);
      case _ => false;
    }
  }

  /**
   * Note that value equality of immutable maps is defined purely in terms
   * of value equality of the *Values*. Keys use their normal equality test.
   */
  implicit def immutableMapsAreEq[K, V](implicit e : Eq[V]) = new Eq[immutable.Map[K, V]]{
    def equal(x : immutable.Map[K, V], y : immutable.Map[K, V]) =
      if (x.size != y.size) false;
      else x.forall((u : (K, V)) => y.get(u._1) match {
        case None => false;
        case Some(v) => e.equal(u._1, v);
      })    
  }
}
