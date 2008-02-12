package sbinary;

import java.io._
import scala.collection._;
import mutable.ListBuffer;

object Sizes{
  val BYTE    = 1;
  val CHAR    = 2;
  val SHORT   = 2;
  val INT     = 4;
  val LONG    = 8;
  val FLOAT   = 4;
  val DOUBLE  = 8;
}

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
   * Read a T from the DataInput, reading no more data than is neccessary.
   */
  def reads(stream : DataInput) :T;

  /**
   * Write a T to the DataOutput. Return the number of bytes written.
   */
  def writes(t : T)(stream : DataOutput) : Int; 
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
   * Use an implicit Binary[T] to read type T from the DataInput.
   */ 
  def read[T](stream : DataInput)(implicit bin : Binary[T]) : T = bin.reads(stream);

  /**
   * Use an implicit Binary[T] to write type T to the DataOutput. 
   */
  def write[T](t : T)(stream : DataOutput)(implicit bin : Binary[T]) : Int = bin.writes(t)(stream);

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
    def reads(stream : DataInput) = stream.readUTF();
    def writes(t : String)(stream : DataOutput) = stream.writeUTF(t);
  }

  implicit object ByteIsBinary extends Binary[Byte]{
    def reads(stream : DataInput) = stream.readByte();
    def writes(t : Byte)(stream : DataOutput) = stream.writeByte(t);
  }

  implicit object CharIsBinary extends Binary[Char]{
    def reads(stream : DataInput) = stream.readChar();
    def writes(t : Char)(stream : DataOutput) = stream.writeChar(t);
  }

  implicit object ShortIsBinary extends Binary[Short]{
    def reads(stream : DataInput) = stream.readShort();
    def writes(t : Short)(stream : DataOutput) = stream.writeShort(t);
  }

  implicit object IntIsBinary extends Binary[Int]{
    def reads(stream : DataInput) = stream.readInt();
    def writes(t : Int)(stream : DataOutput) = stream.writeInt(t);
  }

  implicit object LongIsBinary extends Binary[Long]{
    def reads(stream : DataInput) = stream.readLong();
    def writes(t : Long)(stream : DataOutput) = stream.writeLong(t);
  }

  implicit object FloatIsBinary extends Binary[Float]{
    def reads(stream : DataInput) = stream.readFloat();
    def writes(t : Float)(stream : DataOutput) = stream.writeFloat(t);
  }

  implicit object DoubleIsBinary extends Binary[Double]{
    def reads(stream : DataInput) = stream.readDouble();
    def writes(t : Double)(stream : DataOutput) = stream.writeDouble(t);
  }

  implicit def arraysAreBinary[T](implicit bin : Binary[T]) : Binary[Array[T]] = new Binary[Array[T]]{
    def reads(stream : DataInput) : Array[T] = {
      val length = read[Int](stream);
      val array = new Array[T](length);
      for (i <- 0 until length){
        array(i) = read[T](stream);
      }
      array;
    }

    def writes(ts : Array[T])(stream : DataOutput) = {
      write(ts.length)(stream);
      for (t <- ts){
        write(t)(stream);
      }
    }
  }

  implicit def listsAreBinary[T](implicit bin : Binary[T]) : Binary[List[T]] = new Binary[List[T]]{
    def reads(stream : DataInput) : List[T] = {
      val length = read[Int](stream);
      val buffer = new ListBuffer[T];
      for (i <-  0 until length){
        buffer += read[T](stream);
      }
      buffer.toList;
    }

    def writes(ts : List[T])(stream : DataOutput) = {
      write(ts.length)(stream);
      for (t <- ts){
        write(t)(stream);
      }
    }
  }

  implicit def immutableMapsAreBinary[S, T](implicit binS : Binary[S], binT : Binary[T]) : Binary[immutable.Map[S, T]] = new Binary[immutable.Map[S, T]]{
    def reads(stream : DataInput) : immutable.Map[S, T] = immutable.Map.empty ++ read[Array[(S, T)]](stream) ;
    def writes(ts : immutable.Map[S, T])(stream : DataOutput) = write(ts.toArray)(stream);
  }

  implicit def optionsAreBinary[S](implicit bin : Binary[S]) : Binary[Option[S]] = new Binary[Option[S]]{
    def reads(stream : DataInput) = stream.readByte() match {
      case 1 => Some(read[S](stream));
      case 0 => None;
    }

    def writes(s : Option[S])(stream : DataOutput) = s match {
      case Some(x) => { stream.writeByte(0x1); write(x)(stream) }
      case None => { stream.writeByte(0x0); }
    }
  }
}
