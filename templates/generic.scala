/**
 * Generic operations for building binary instances.
 */
package sbinary.generic;
import sbinary.Operations._;
import java.io._;

trait Split[S, L, R]{
  def join(l  : L, r : R) : S;
  def fold[T](f : (L, R) => T)(s : S) : T;
}

object View{
  def invert[S, T](view : View[S, T]) = new View[T, S]{
    def to(t : T) : S = view.from(t);
    def from(s : S) : T = view.to(s);
  }

  def compose[S, T, U](view1 : View[S, T], view2 : View[T, U]) : View[S, U] = new View[S, U]{
    def to(s : S) = view2.to(view1.to(s));
    def from(u : U) = view1.from(view2.from(u));
  }
}

trait View[S, T]{
  def to(s : S) : T;
  def from(t : T) : S;
}

// Unions are abstract classes rather than traits due to implementation reasons.
<#list 2..22 as i>
abstract class Union${i}[S, <#list 1..i as j>T${j} <% S<#if i!=j>, </#if></#list>]{
  def fold[U] (
<#list 1..i as j>
      f${j} : T${j} => U<#if i!=j>,</#if>
</#list>
    ) : S => U
  }
</#list>

object Operations{
  def via[S, T](implicit view : View[S, T], bin : Binary[T]) = new Binary[S]{
    def reads(input : DataInput) : S = view.from(read[T](input));
    def writes(s : S)(output : DataOutput) = write[T](view.to(s))(output);
  }

}
