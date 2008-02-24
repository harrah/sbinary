package sbinary;

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
 * their contents are equal.  
 * 
 * Instances must be independent of platform and network byte order.  
 */
trait Binary[T]{
  /**
   * Read a T from the DataInput, reading no more data than is neccessary.
   */
  def reads(stream : DataInput) : T;

  /**
   * Write a T to the DataOutput. Return the number of bytes written.
   */
  def writes(t : T)(stream : DataOutput) : Unit; 
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
   * Use an implicit Binary[T] to write type T to the DataOutput. 
   */
  def write[T](t : T)(stream : DataOutput)(implicit bin : Binary[T]) : Unit = bin.writes(t)(stream);

  /**
   * Get the serialized value of this class as a byte array.
   */
  def toByteArray[T](t : T)(implicit bin : Binary[T]) : Array[Byte] = {
    val target = new ByteArrayOutputStream();
    write(t)(target);
    target.toByteArray(); 
  }
 
  /**
   * Read a value from the byte array. Anything past the end of the value will be
   * ignored.
   */ 
  def fromByteArray[T](array : Array[Byte])(implicit bin : Binary[T]) = read[T](new ByteArrayInputStream(array));

  /** 
   * Convenience method for writing binary data to a file.
   */
  def toFile[T](t : T)(file : File)(implicit bin : Binary[T]) = {
    val raf = new RandomAccessFile(file, "rw");
    try{
      write[T](t)(raf);}
    finally{
      raf.close(); }
  }

  /** 
   * Convenience method for reading binary data from a file.
   */
  def fromFile[T](file : File)(implicit bin : Binary[T]) = {
    val raf = new RandomAccessFile(file, "rw");
    try{
      read[T](raf);}
    finally{
      raf.close(); }
  }
}

/**
 * Implicit instances for many standard types.
 */
object Instances{
  import Operations._;

  implicit object UnitIsBinary extends Binary[Unit]{
    def reads(stream : DataInput) = ((), 0);
    def writes(t : Unit)(stream : DataOutput) = ();
  }

  implicit object StringIsBinary extends Binary[String]{
    def reads(stream : DataInput) = stream.readUTF();
    def writes(t : String)(stream : DataOutput) =
      stream.writeUTF(t);
  }

  implicit object BooleanIsBinary extends Binary[Boolean]{
    def reads(stream : DataInput) = stream.readByte != 0
    def writes(t : Boolean)(stream : DataOutput) = 
      if (t) write[Byte](0x01)(stream) 
      else write[Byte](0x00)(stream);
  }

  implicit object ByteIsBinary extends Binary[Byte]{
    def reads(stream : DataInput) = stream.readByte()
    def writes(t : Byte)(stream : DataOutput) = 
      stream.writeByte(t);
  }

  implicit object CharIsBinary extends Binary[Char]{
    def reads(stream : DataInput) = stream.readChar()
    def writes(t : Char)(stream : DataOutput) = 
      stream.writeChar(t);
  }

  implicit object ShortIsBinary extends Binary[Short]{
    def reads(stream : DataInput) = stream.readShort()
    def writes(t : Short)(stream : DataOutput) = 
      stream.writeShort(t);
  }

  implicit object IntIsBinary extends Binary[Int]{
    def reads(stream : DataInput) = stream.readInt()
    def writes(t : Int)(stream : DataOutput) = 
      stream.writeInt(t);
    
  }

  implicit object LongIsBinary extends Binary[Long]{
    def reads(stream : DataInput) = stream.readLong();
    def writes(t : Long)(stream : DataOutput) = 
      stream.writeLong(t);
  }

  implicit object FloatIsBinary extends Binary[Float]{
    def reads(stream : DataInput) = stream.readFloat()
    def writes(t : Float)(stream : DataOutput) = 
      stream.writeFloat(t);
  }

  implicit object DoubleIsBinary extends Binary[Double]{
    def reads(stream : DataInput) = stream.readDouble()
    def writes(t : Double)(stream : DataOutput) = 
      stream.writeDouble(t);
  }

<#list 2..22 as i>
  <#assign typeName>
   Tuple${i}[<#list 1..i as j>T${j} <#if i != j>,</#if></#list>]
  </#assign>
  implicit def tuple${i}sAreBinary[<#list 1..i as j>T${j}<#if i !=j>,</#if></#list>](implicit 
    <#list 1..i as j>
      bin${j} : Binary[T${j}] <#if i != j>,</#if>
    </#list>
    ) : Binary[${typeName}] = new Binary[${typeName}]{
      def reads (input : DataInput) : ${typeName} = ( 
    <#list 1..i as j>
        read[T${j}](input)<#if i!=j>,</#if>
    </#list>
      )

      def writes(tuple : ${typeName})(output : DataOutput) = { 
      <#list 1..i as j>
        write(tuple._${j})(output);      
      </#list>;
      }
  }
</#list>

  import sbinary.generic.Building._;
  import sbinary.generic.Generic._;

  implicit def listsAreBinary[T](implicit bin : Binary[T]) : Binary[List[T]] = lengthEncoded[T, List] 
  implicit def arraysAreBinary[T](implicit bin : Binary[T]) : Binary[Array[T]] = lengthEncoded[T, Array]
 
  implicit def immutableMapsAreBinary[S, T](implicit binS : Binary[S], binT : Binary[T]) : Binary[immutable.Map[S, T]] = new Binary[immutable.Map[S, T]]{
    def reads(stream : DataInput) = 
      immutable.Map.empty ++ read[Array[(S, T)]](stream)
    def writes(ts : immutable.Map[S, T])(stream : DataOutput) = write(ts.toArray)(stream);
  }

  implicit def optionsAreBinary[S](implicit bin : Binary[S]) : Binary[Option[S]] = new Binary[Option[S]]{
    def reads(stream : DataInput) = stream.readByte() match {
      case 1 => Some(read[S](stream));
      case 0 => None
    }

    def writes(s : Option[S])(stream : DataOutput) = s match {
      case Some(x) => { write[Byte](0x1)(stream); write(x)(stream) }
      case None => write[Byte](0x0)(stream);
    }
  }
}
