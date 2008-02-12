package sbinary.equality;

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
}
