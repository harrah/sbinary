package sbintest

import sbinary._
import DefaultProtocol._
import Operations._

	// Binary tree data types that will be
	//   created and converted to/from byte arrays

sealed abstract class BT;
case class Bin(left : BT, right : BT) extends BT;
case class Leaf(label : String) extends BT;

	// Custom protocol for (de)serializing binary trees

object MyProtocol extends DefaultProtocol {
	implicit object BTFormat extends Format[BT]{
		def reads(in : Input) = read[Byte](in) match {
			case 0 => Bin(reads(in), reads(in));
			case _ => Leaf(read[String](in))
		}

		def writes(out : Output, value : BT) = value match {
			case Bin(left, right) => write[Byte](out, 0); writes(out, left); writes(out, right);
			case Leaf(label) => write[Byte](out, 1); write(out, label);
		}
	}
}

	// test runner

object main {
	def main(args : Array[String]) : Unit = {
		val input = List("foo", "bar", "baz")
		println(input)

		val binary = toByteArray(input)
		println(showArray(binary))

		val normalAgain = fromByteArray[List[String]](binary)
		println(normalAgain)

		toFile(input)(new java.io.File("foo.bin"))
		println(fromFile[List[String]](new java.io.File("foo.bin")))

		import MyProtocol._

		val someTree = Bin(Leaf("foo"), Bin(Leaf("bar"), Leaf("baz")))
		println(someTree)

		val someTreeInAByteArray = toByteArray[BT](someTree)
		println(showArray(someTreeInAByteArray))

		val theSameTree = fromByteArray[BT](someTreeInAByteArray)
		println(theSameTree)
	}
	// utility methods for printing a byte array
	def showArray(b: Array[Byte]) = b.map(showByte).mkString(" ")
	def showByte(b: Byte) = pad( ((b+256) % 256).toHexString )
	def pad(s: String) = if(s.length == 1) "0" + s else s
}