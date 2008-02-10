package binary;

import java.io._
import scala.collection._;
import mutable.ListBuffer;

/**
 * Trait for marshaling type T to and from binary data. 
 *
 * Because of the possibility of marshalling mutable types this library 
 * doesn't make any strong guarantees about equality. In general implementations
 * should make a best effort to ensure that read(write(t)) is in some sense equal
 * to t. This sense should be == if possible, but e.g. arrays are chosen so that
 * 
 * 
 * Instances must be independent of platform and network byte order.  
 */
trait Binary[T]{
  /**
   * Read a T from the DataInputStream, reading no more data than is neccessary.
   */
  def reads(stream : DataInputStream) :T;

  /**
   * Write a T to the DataOutputStream.
   */
  def writes(t : T)(stream : DataOutputStream) : Unit; 
}

object Operations{
  implicit def wrapOutputStream(stream : OutputStream) : DataOutputStream = stream match {
    case (x : DataOutputStream) => x;
    case y => new DataOutputStream(y);
  }
    
  implicit def wrapInputStream(stream : InputStream) : DataInputStream = stream match {
    case (x : DataInputStream) => x;
    case y => new DataInputStream(y);
  }
 
  /**
   * Use an implicit Binary[T] to read type T from the DataInputStream.
   */ 
  def read[T](stream : DataInputStream)(implicit bin : Binary[T]) : T = bin.reads(stream);

  /**
   * Use an implicit Binary[T] to write type T to the DataOutputStream.
   */
  def write[T](t : T)(stream : DataOutputStream)(implicit bin : Binary[T]) : Unit = bin.writes(t)(stream);

  def toByteArray[T](t : T)(implicit bin : Binary[T]) : Array[Byte] = {
    val target = new ByteArrayOutputStream();
    write(t)(target);
    target.toByteArray(); 
  }
  
  def fromByteArray[T](array : Array[Byte])(implicit bin : Binary[T]) = read[T](new ByteArrayInputStream(array));
}

object Instances{
  import Operations._;
  import TupleInstances._;

  implicit object StringIsBinary extends Binary[String]{
    def reads(stream : DataInputStream) = stream.readUTF();
    def writes(t : String)(stream : DataOutputStream) = stream.writeUTF(t);
  }

  implicit object ByteIsBinary extends Binary[Byte]{
    def reads(stream : DataInputStream) = stream.readByte();
    def writes(t : Byte)(stream : DataOutputStream) = stream.writeByte(t);
  }

  implicit object CharIsBinary extends Binary[Char]{
    def reads(stream : DataInputStream) = stream.readChar();
    def writes(t : Char)(stream : DataOutputStream) = stream.writeChar(t);
  }

  implicit object ShortIsBinary extends Binary[Short]{
    def reads(stream : DataInputStream) = stream.readShort();
    def writes(t : Short)(stream : DataOutputStream) = stream.writeShort(t);
  }

  implicit object IntIsBinary extends Binary[Int]{
    def reads(stream : DataInputStream) = stream.readInt();
    def writes(t : Int)(stream : DataOutputStream) = stream.writeInt(t);
  }

  implicit object LongIsBinary extends Binary[Long]{
    def reads(stream : DataInputStream) = stream.readLong();
    def writes(t : Long)(stream : DataOutputStream) = stream.writeLong(t);
  }

  implicit object FloatIsBinary extends Binary[Float]{
    def reads(stream : DataInputStream) = stream.readFloat();
    def writes(t : Float)(stream : DataOutputStream) = stream.writeFloat(t);
  }

  implicit object DoubleIsBinary extends Binary[Double]{
    def reads(stream : DataInputStream) = stream.readDouble();
    def writes(t : Double)(stream : DataOutputStream) = stream.writeDouble(t);
  }

  implicit def arraysAreBinary[T](implicit bin : Binary[T]) : Binary[Array[T]] = new Binary[Array[T]]{
    def reads(stream : DataInputStream) : Array[T] = {
      val length = read[Int](stream);
      val array = new Array[T](length);
      for (i <- 0 until length){
        array(i) = read[T](stream);
      }
      array;
    }

    def writes(ts : Array[T])(stream : DataOutputStream) = {
      write(ts.length)(stream);
      for (t <- ts){
        write(t)(stream);
      }
    }
  }

  implicit def listsAreBinary[T](implicit bin : Binary[T]) : Binary[List[T]] = new Binary[List[T]]{
    def reads(stream : DataInputStream) : List[T] = {
      val length = read[Int](stream);
      val buffer = new ListBuffer[T];
      for (i <-  0 until length){
        buffer += read[T](stream);
      }
      buffer.toList;
    }

    def writes(ts : List[T])(stream : DataOutputStream) = {
      write(ts.length)(stream);
      for (t <- ts){
        write(t)(stream);
      }
    }
  }

  implicit def immutableMapsAreBinary[S, T](implicit binS : Binary[S], binT : Binary[T]) : Binary[immutable.Map[S, T]] = new Binary[immutable.Map[S, T]]{
    def reads(stream : DataInputStream) : immutable.Map[S, T] = immutable.Map.empty ++ read[Array[(S, T)]](stream) ;
    def writes(ts : immutable.Map[S, T])(stream : DataOutputStream) = write(ts.toArray)(stream);
  }

  implicit def optionsAreBinary[S](implicit bin : Binary[S]) : Binary[Option[S]] = new Binary[Option[S]]{
    def reads(stream : DataInputStream) = stream.readByte() match {
      case 1 => Some(read[S](stream));
      case 0 => None;
    }

    def writes(s : Option[S])(stream : DataOutputStream) = s match {
      case Some(x) => { stream.writeByte(0x1); write(x)(stream) }
      case None => { stream.writeByte(0x0); }
    }
  }
}
