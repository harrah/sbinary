package sbinary;
import org.scalacheck._;
import org.scalacheck.Test._;
import Gen._;
import Arbitrary._;
import Prop._;

import scala.collection._;
import Operations._;
import Instances._;
import TupleInstances._;

import scalaz.Equal;
import scalaz.Equal._;
import scalazextensions.EqualityInstances._;

object BinaryTests extends Application{
  def test[T](prop : T => Boolean)(implicit arb : Arb[T] => Arbitrary[T]) = {
    check(property(prop)).result match {
      case GenException(e) => e.printStackTrace();
      case _ => ();
    }
  }

  def testBinaryProperties[T](name : String)(implicit 
                               bin : Binary[T], 
                               arb : Arb[T] => Arbitrary[T],
                             equal : Equal[T]) = {
    println(name);
    test((x : T) => equal(x, fromByteArray[T](toByteArray(x))))
    testLength[T];
//    testBinaryTypePreservesArrayEquality[T]("Array[" + name + "]");
//    testBinaryTypePreservesArrayEquality[Array[T]]("Array[Array[" + name + "]]");
  }

  def testLength[T](implicit bin : Binary[T], arb : Arb[T] => Arbitrary[T]) = {
    test((x : T) => {
      val stream = new java.io.ByteArrayOutputStream;
      write[T](x)(stream) == stream.toByteArray.length     
    })    
  }

  implicit def arbitraryUnit(x : Arb[Unit]) = new Arbitrary[Unit]{
    def getArbitrary = value(() => ());
  }

  implicit def arbitraryArray[T](x: Arb[Array[T]])(implicit arb : Arb[T] => Arbitrary[T]): Arbitrary[Array[T]] =
    new Arbitrary[Array[T]] {
      def getArbitrary = arbitrary[List[T]] map ((_.toArray))
    }

  implicit def arbitraryMap[K, V](arb : Arb[immutable.Map[K, V]])(implicit arbK : Arb[K] => Arbitrary[K], arbV : Arb[V] => Arbitrary[V]) : Arbitrary[immutable.Map[K, V]]=
    new Arbitrary[immutable.Map[K, V]]{
    def getArbitrary = arbitrary[List[(K, V)]].map(x => immutable.Map.empty ++ x);
  }

  println("Primitives");
  testBinaryProperties[Byte]("Byte");
  testBinaryProperties[Char]("Char");
  testBinaryProperties[Int]("Int");
  testBinaryProperties[Double]("Double");

  // No Arbitrary instances for these. Write some.
  // testBinaryProperties[Long]("Long");
  // testBinaryProperties[Short]("Short");
  // testBinaryProperties[Float]("Float");

  println
  testBinaryProperties[Unit]("Unit");

  println
  testBinaryProperties[String]("String")

  println ("Tuples")
  testBinaryProperties[(Int, Int, Int)]("(Int, Int, Int)");
  testBinaryProperties[(String, Int, String)]("(String, Int, String)")
  testBinaryProperties[((Int, (String, Int), Int))]("((Int, (String, Int), Byte, Byte, Int))]");

  println
  println("Options");
  testBinaryProperties[Option[String]]("Option[String]");
  testBinaryProperties[(Option[String], String)]("(Option[String], String)");
  testBinaryProperties[Option[Option[Int]]]("Option[Option[Int]]");
  
  println
  println("Lists");
  testBinaryProperties[List[String]]("List[String]");
  testBinaryProperties[List[(String, Int)]]("List[(String, Int)]");
  testBinaryProperties[List[Option[Int]]]("List[Option[Int]]");
  testBinaryProperties[List[Unit]]("List[Unit]");

  println
  println("Arrays");
  testBinaryProperties[String]("Array[String]");
  testBinaryProperties[Array[String]]("Array]Array[String]]");
  testBinaryProperties[List[Int]]("Array[List[Int]]");
  testBinaryProperties[Option[Byte]]("Array[Option[Byte]]");
  testBinaryProperties[Byte]("Array[Byte]");
  testBinaryProperties[(Int, Int)]("Array[(Int, Int)]");

  println
  println("Maps");
  testBinaryProperties[immutable.Map[Int, Int]]("immutable.Map[Int, Int]");
  testBinaryProperties[immutable.Map[Int, Int]]("immutable.Map[Option[String], Int]");
  testBinaryProperties[immutable.Map[Int, Int]]("immutable.Map[List[Int], String]");
}
