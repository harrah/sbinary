package sbinary;

import java.io._
import scala.collection._;
import mutable.ListBuffer;

object Sizes{
  val BOOLEAN = 1;
  val BYTE    = 1;
  val CHAR    = 2;
  val SHORT   = 2;
  val INT     = 4;
  val LONG    = 8;
  val FLOAT   = 4;
  val DOUBLE  = 8;

  def sizeAsUTF(t : String) = 
      t.foldLeft(2)((i, c) => 
        if ((c >= 0x0001) && (c <= 0x007F)) i + 1
        else if (c > 0x07FF) i + 3
        else i + 2)
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
  def reads(stream : DataInput) : T = readsWithSize(stream)._1;

  /**
   * Read a T from the DataInput, reading no more data than is neccessary.
   * Return the read instance together with the size of data read.
   */
  def readsWithSize(stream : DataInput) : (T, Int);

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
 
  implicit def fileByName(name : String) : File = new File(name);

  /**
   * Use an implicit Binary[T] to read type T from the DataInput.
   */ 
  def read[T](stream : DataInput)(implicit bin : Binary[T]) : T = bin.reads(stream);

  /**
   * Use an implicit Binary[T] to read type T from the DataInput.
   */ 
  def readWithSize[T](stream : DataInput)(implicit bin : Binary[T]) : (T, Int) = bin.readsWithSize(stream);

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

  def toFile[T](t : T)(file : File)(implicit bin : Binary[T]) = {
    val raf = new RandomAccessFile(file, "rw");
    try{
      write[T](t)(raf);}
    finally{
      raf.close(); }
  }

  def fromFile[T](file : File)(implicit bin : Binary[T]) = {
    val raf = new RandomAccessFile(file, "rw");
    try{
      read[T](raf);}
    finally{
      raf.close(); }
  }
  
}
object Instances{
  import Operations._;
  import TupleInstances._;
  import Sizes.sizeAsUTF;

  implicit object UnitIsBinary extends Binary[Unit]{
    def readsWithSize(stream : DataInput) = ((), 0);
    def writes(t : Unit)(stream : DataOutput) = 0;
  }

  implicit object StringIsBinary extends Binary[String]{
    def readsWithSize(stream : DataInput) = {val string = stream.readUTF(); (string, sizeAsUTF(string)); }
    def writes(t : String)(stream : DataOutput) = { 
      stream.writeUTF(t);
      // Note that this duplicates some work done in the writing
      // logic. Sigh. Might want to add pattern matching so we can
      // avoid this duplication in common cases - e.g. DataOutputStream
      // and RandomAccessFile.
      sizeAsUTF(t);
    }
  }

  implicit object BooleanIsBinary extends Binary[Boolean]{
    def readsWithSize(stream : DataInput) = (stream.readByte != 0, Sizes.BOOLEAN);
    def writes(t : Boolean)(stream : DataOutput) = 
      if (t) write[Byte](0x01)(stream) 
      else write[Byte](0x00)(stream);
  }

  implicit object ByteIsBinary extends Binary[Byte]{
    def readsWithSize(stream : DataInput) = (stream.readByte(), Sizes.BYTE);
    def writes(t : Byte)(stream : DataOutput) = {
      stream.writeByte(t);
      Sizes.BYTE;
    }
  }

  implicit object CharIsBinary extends Binary[Char]{
    def readsWithSize(stream : DataInput) = (stream.readChar(), Sizes.CHAR);
    def writes(t : Char)(stream : DataOutput) = {
      stream.writeChar(t);
      Sizes.CHAR;
    }
  }

  implicit object ShortIsBinary extends Binary[Short]{
    def readsWithSize(stream : DataInput) = (stream.readShort(), Sizes.SHORT);
    def writes(t : Short)(stream : DataOutput) = {
      stream.writeShort(t);
      Sizes.SHORT;
    }
  }

  implicit object IntIsBinary extends Binary[Int]{
    def readsWithSize(stream : DataInput) = (stream.readInt(), Sizes.INT);
    def writes(t : Int)(stream : DataOutput) = {
      stream.writeInt(t);
      Sizes.INT;
    }
  }

  implicit object LongIsBinary extends Binary[Long]{
    def readsWithSize(stream : DataInput) = (stream.readLong(), Sizes.LONG);
    def writes(t : Long)(stream : DataOutput) = {
      stream.writeLong(t);
      Sizes.LONG;
    }
  }

  implicit object FloatIsBinary extends Binary[Float]{
    def readsWithSize(stream : DataInput) = (stream.readFloat(), Sizes.FLOAT);
    def writes(t : Float)(stream : DataOutput) = {
      stream.writeFloat(t);
      Sizes.FLOAT;
    }
  }

  implicit object DoubleIsBinary extends Binary[Double]{
    def readsWithSize(stream : DataInput) = (stream.readDouble(), Sizes.DOUBLE);
    def writes(t : Double)(stream : DataOutput) = {
      stream.writeDouble(t);
      Sizes.DOUBLE;
    }
  }

  implicit def arraysAreBinary[T](implicit bin : Binary[T]) : Binary[Array[T]] = new Binary[Array[T]]{
    def readsWithSize(stream : DataInput) : (Array[T], Int) = {
      val length = read[Int](stream);
      val array = new Array[T](length);
      var size = 0;
      for (i <- 0 until length){
        val (value, s) = readWithSize[T](stream);
        array(i) = value;
        size = size + s;
      }
      (array, size);
    }

    def writes(ts : Array[T])(stream : DataOutput) = {
      write(ts.length)(stream);
      ts.foldLeft(Sizes.INT)((i, t) => i + write(t)(stream));
    }
  }

  implicit def listsAreBinary[T](implicit bin : Binary[T]) : Binary[List[T]] = new Binary[List[T]]{
    def readsWithSize(stream : DataInput) : (List[T], Int) = {
      val length = read[Int](stream);
      val buffer = new ListBuffer[T];
      var size = 0;
      for (i <-  0 until length){
        val (value, s) = readWithSize[T](stream);
        buffer += value;
        size = size + s;
      }
      (buffer.toList, size);
    }

    def writes(ts : List[T])(stream : DataOutput) = {
      write(ts.length)(stream);
      ts.foldLeft(Sizes.INT)((i, t) => i + write(t)(stream));
    }
  }

  implicit def immutableMapsAreBinary[S, T](implicit binS : Binary[S], binT : Binary[T]) : Binary[immutable.Map[S, T]] = new Binary[immutable.Map[S, T]]{
    def readsWithSize(stream : DataInput) : (immutable.Map[S, T], Int) = {
      val (value, size) = readWithSize[Array[(S, T)]](stream);
      (immutable.Map.empty ++ value, size)
    }
    def writes(ts : immutable.Map[S, T])(stream : DataOutput) = write(ts.toArray)(stream);
  }

  implicit def optionsAreBinary[S](implicit bin : Binary[S]) : Binary[Option[S]] = new Binary[Option[S]]{
    def readsWithSize(stream : DataInput) = stream.readByte() match {
      case 1 => {val (value, size) = readWithSize[S](stream); (Some(value), size) };
      case 0 => (None, Sizes.BYTE);
    }

    def writes(s : Option[S])(stream : DataOutput) = s match {
      case Some(x) => write[Byte](0x1)(stream) + write(x)(stream)
      case None => write[Byte](0x0)(stream);
    }
  }
}
