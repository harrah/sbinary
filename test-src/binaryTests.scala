package sbinary;
import org.scalacheck._;
import org.scalacheck.Test._;
import Gen._;
import Arbitrary._;
import Prop._;

import scala.collection._;
import DefaultProtocol._;

object Equal{
  abstract class Equal[T] extends Function2[T, T, Boolean];

  def allAreEqual[T] : Equal[T] = new Equal[T]{
    def apply(x : T, y : T) = x == y
  }

  implicit val eqString = allAreEqual[String];
  implicit val eqInt = allAreEqual[Int];
  implicit val eqLong = allAreEqual[Long];
  implicit val eqBoolean = allAreEqual[Boolean];
  implicit val eqUnit = allAreEqual[Unit];
  implicit val eqByte = allAreEqual[Byte];
  implicit val eqDouble = allAreEqual[Double];
  implicit val eqChar = allAreEqual[Char];
  implicit def eqSet[T] = allAreEqual[immutable.Set[T]];
  implicit def eqSortedSet[T] = allAreEqual[immutable.SortedSet[T]];
  implicit def eqMap[S,T] = allAreEqual[immutable.Map[S,T]];
  implicit def eqSortedMap[S,T] = allAreEqual[immutable.SortedMap[S,T]];

  implicit def eqList[T](implicit eqT : Equal[T]) : Equal[List[T]] = new Equal[List[T]]{
    def apply(x : List[T], y : List[T]) = (x, y) match {
      case (Nil, Nil) => true;
      case (x :: xs, y :: ys) => eqT(x, y) && apply(xs, ys);
      case _ => false;
    }
  }

  implicit def eqOption[T](implicit eqT : Equal[T]) : Equal[Option[T]] = new Equal[Option[T]]{
    def apply(x : Option[T], y : Option[T]) = (x, y) match {
      case (None, None) => true;
      case (Some(u), Some(v)) => eqT(u, v); 
      case _ => false;
    }
  }

  implicit def eqTuple3[S, T, U](implicit eqS : Equal[S], eqT : Equal[T], eqU : Equal[U]) : Equal[(S, T, U)] = new Equal[(S, T, U)]{
    def apply(x : (S, T, U), y : (S, T, U)) = eqS(x._1, y._1) && eqT(x._2, y._2) && eqU(x._3, y._3);
  }

  implicit def eqTuple2[S, T](implicit eqS : Equal[S], eqT : Equal[T]) : Equal[(S, T)] = new Equal[(S, T)]{
    def apply(x : (S, T), y : (S, T)) = eqS(x._1, y._1) && eqT(x._2, y._2)
  }

  implicit def arraysAreEqual[T](implicit e : Equal[T]) : Equal[Array[T]] = new Equal[Array[T]]{
    def apply(x : Array[T], y : Array[T]) = (x.length == y.length) &&  x.zip(y).forall({case (x, y) => e(x, y)})
  }

  implicit def streamsAreEqual[T](implicit e : Equal[T]) : Equal[Stream[T]] = new Equal[Stream[T]]{
    def apply(x : Stream[T], y : Stream[T]) = (x.length == y.length) &&  x.zip(y).forall({case (x, y) => e(x, y)})
  }

  def equal[T](x : T, y : T)(implicit f : Equal[T]) = f(x, y);


}

import Equal._;

object StringTests extends Properties("Strings"){
  import java.io._;

  specify("AgreesWithDataInput", (x : String) => {
    val it = new ByteArrayOutputStream();
    try { write(it, x); (new DataInputStream(new ByteArrayInputStream(it.toByteArray))).readUTF == x }
    catch { case (e : Throwable) => e.printStackTrace; false }
  });

  specify("AgreesWithDataOutput", (x : String) => {    
    val it = new ByteArrayOutputStream();
    try { 
      (new DataOutputStream(it)).writeUTF(x); 
      val ba = it.toByteArray;
      fromByteArray[String](ba).length == x.length
    } catch { case (e : Throwable) => e.printStackTrace; false }
  })
}


object LazyIOTests extends Properties("LazyIO"){

  import java.io._;
  specify("NoMixingOfStreams", (x : Stream[Int]) => { 
    val (u, v) = fromByteArray[(Stream[Int], Stream[Int])](toByteArray((x, x)))
    equal(u, v) && equal(u, x); 
  });

  specify("NoMixingOfStreamsAndOthers", (x : Stream[Int], y : String, z : Stream[Int]) => { 
    val (u, s, v) = fromByteArray[(Stream[Int], String, Stream[Int])](toByteArray((x, y, z)))
    equal(u, x) && equal(s, y) && equal(z, v); 
  });

  specify("StreamsOfStreams", (x : Stream[Stream[Int]]) => !x.isEmpty ==> {
    val x2 = fromByteArray[Stream[Stream[Int]]](toByteArray(x));
    equal(x(x.length - 1), x2(x.length - 1));
  });
}

object FormatTests extends Properties("Binaries"){
  def validFormat[T](implicit 
                     bin : Format[T], 
                     arb : Arbitrary[T],
                    equal : Equal[T]) = property((x : T) => 
      try { equal(x, fromByteArray[T](toByteArray(x))) } catch {
        case (e : Throwable) => e.printStackTrace; false 
      })

  def formatSpec[T](name : String)(implicit 
                     bin : Format[T], 
                     arb : Arbitrary[T],
                    equal : Equal[T]) = 
    specify(name, validFormat[T]) 

  implicit val arbitraryUnit = Arbitrary[Unit](value(() => ()))

  implicit def arbitrarySortedMap[K, V](implicit ord : K => Ordered[K], arbK : Arbitrary[K], arbV : Arbitrary[V]) : Arbitrary[immutable.SortedMap[K, V]] =  Arbitrary(arbitrary[List[(K, V)]].map(x => immutable.TreeMap(x :_*)))

  implicit def arbitrarySet[T](implicit arb : Arbitrary[T]) : Arbitrary[immutable.Set[T]] = Arbitrary(arbitrary[List[T]].map((x : List[T]) => immutable.Set(x :_*)));

  implicit val arbitraryEnumeration : Arbitrary[Enumeration] = Arbitrary(arbitrary[List[String]].map(x => new Enumeration(x : _*){}));

  implicit def orderedOption[T](opt : Option[T])(implicit ord : T => Ordered[T]) : Ordered[Option[T]] = new Ordered[Option[T]]{
    def compare(that : Option[T]) = (opt, that) match {
      case (None, None) => 0;
      case (None, Some(_)) => -1;
      case (Some(_), None) => 1;
      case (Some(x), Some(y)) => x.compare(y);
    }
  }

  trait Foo;

  case class Bar extends Foo;
  case class Baz (string : String) extends Foo;

  implicit val eqFoo = allAreEqual[Foo]

  implicit val BarIsFormat : Format[Bar] = asSingleton(Bar())
  implicit val BazIsFormat : Format[Baz] = viaString(Baz)
  implicit val FooIsFormat  : Format[Foo] = asUnion[Foo](classOf[Bar], classOf[Baz])



  implicit val arbitraryFoo : Arbitrary[Foo] = Arbitrary[Foo](arbitrary[Boolean].flatMap( (bar : Boolean) =>
                            if (bar) value(Bar) else arbitrary[String].map(Baz(_))))
  


  sealed abstract class BinaryTree;
  case class Split(left : BinaryTree, right : BinaryTree) extends BinaryTree;
  case class Leaf extends BinaryTree;

  implicit val eqBinaryTree = allAreEqual[BinaryTree]

  implicit val BinaryTreeIsFormat : Format[BinaryTree] = lazyFormat({
    implicit val formatLeaf = asSingleton(Leaf());

    implicit val formatSplit : Format[Split] = asProduct2((x : BinaryTree, y : BinaryTree) => Split(x, y))((s : Split) => (s.left, s.right));
    asUnion[BinaryTree](classOf[Leaf], classOf[Split]);
  })

  implicit val arbitraryTree : Arbitrary[BinaryTree] = {
    def sizedArbitraryTree(n : Int) : Gen[BinaryTree] = 
      if (n <= 1) value(Leaf());
      else for (i <- choose(1, n - 1);
                left <- sizedArbitraryTree(i);
                right <- sizedArbitraryTree(n - i)
               ) yield (Split(left, right));
    Arbitrary[BinaryTree](sized(sizedArbitraryTree(_ : Int)))
  }

  formatSpec[Boolean]("Boolean");
  formatSpec[Byte]("Byte");
  formatSpec[Char]("Char");
  formatSpec[Int]("Int");
  formatSpec[Double]("Double");
  formatSpec[Long]("Long");

  formatSpec[Unit]("Unit");

  formatSpec[String]("String")

  formatSpec[(Int, Int, Int)]("(Int, Int, Int)");
  formatSpec[(String, Int, String)]("(String, Int, String)")
  formatSpec[((Int, (String, Int), Int))]("((Int, (String, Int), Byte, Byte, Int))]");
  formatSpec[(String, String)]("(String, String)")

  formatSpec[Option[String]]("Option[String]");
  formatSpec[(Option[String], String)]("(Option[String], String)");
  formatSpec[Option[Option[Int]]]("Option[Option[Int]]");
  
  formatSpec[List[String]]("List[String]");
  formatSpec[List[(String, Int)]]("List[(String, Int)]");
  formatSpec[List[Option[Int]]]("List[Option[Int]]");
  formatSpec[List[Unit]]("List[Unit]");

  formatSpec[immutable.Set[String]]("immutable.Set[String]");
  formatSpec[immutable.Set[(String, Int)]]("immutable.Set[(String, Int)]");
  formatSpec[immutable.Set[Option[Int]]]("immutable.Set[Option[Int]]");
  formatSpec[immutable.Set[Unit]]("immutable.Set[Unit]");

  formatSpec[String]("Array[String]");
  formatSpec[Array[String]]("Array[String]]");
  formatSpec[Array[List[Int]]]("Array[List[Int]]");
  formatSpec[Array[Stream[Int]]]("Array[Stream[Int]]");
  formatSpec[Array[Option[Byte]]]("Array[Option[Byte]]");
  formatSpec[Array[Byte]]("Array[Byte]");
  formatSpec[Array[(Int, Int)]]("Array[(Int, Int)]");

  formatSpec[String]("Stream[String]");
  formatSpec[Stream[String]]("Stream]Stream[String]]");
  formatSpec[Stream[List[Int]]]("Stream[List[Int]]");
  formatSpec[Stream[Option[Byte]]]("Stream[Option[Byte]]");
  formatSpec[Stream[Byte]]("Stream[Byte]");
  formatSpec[Stream[(Int, Int)]]("Stream[(Int, Int)]");
  formatSpec[Stream[Stream[Int]]]("Stream[Stream[Int]]");

  formatSpec[immutable.Map[Int, Int]]("immutable.Map[Int, Int]");
  formatSpec[immutable.Map[Option[String], Int]]("immutable.Map[Option[String], Int]");
  formatSpec[immutable.Map[List[Int], Int]]("immutable.Map[List[Int], String]");

  formatSpec[immutable.SortedMap[Int, Int]]("immutable.SortedMap[Int, Int]");
  formatSpec[immutable.SortedMap[String, Int]]("immutable.SortedMap[String, Int]");
  formatSpec[immutable.SortedMap[Option[String], Int]]("immutable.SortedMap[Option[String], Int]");

  formatSpec[Foo]("Foo")
  formatSpec[(Foo, Foo)]("(Foo, Foo)")
  formatSpec[Array[Foo]]("Array[Foo]")

  formatSpec[BinaryTree]("BinaryTree");
  formatSpec[(BinaryTree, BinaryTree)]("(BinaryTree, BinaryTree)")

  include(LazyIOTests);
}
