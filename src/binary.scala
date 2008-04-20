package sbinary;

import java.io._
import scala.collection._;
import scala.collection.jcl.IdentityHashMap;
import immutable.TreeMap;
import mutable.ListBuffer;
import Instances._;

class Input private[sbinary] (private[sbinary] val source : DataInput){
  def read[S](implicit bin : Binary[S]) : S = bin.reads(this); 
}

class Output private[sbinary] (private[sbinary] val source : DataOutput){
  def write[T](t : T)(implicit bin : Binary[T]) : Unit = bin.writes(t)(this);  
}

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
   * Read a T from the Input, reading no more data than is neccessary.
   */
  def reads(in : Input) : T;

  /**
   * Write a T to the Output. Return the number of bytes written.
   */
  def writes(t : T)(out : Output) : Unit; 
}

/**
 * Standard operations on binary types
 */
object Operations{
  implicit def wrapOutputStream(out : OutputStream) : Output = out match {
    case (x : DataOutputStream) => new Output(x);
    case y => new Output(new DataOutputStream(y));
  }
    
  implicit def wrapInputStream(in : InputStream) : Input = in match {
    case (x : DataInputStream) => new Input(x);
    case y => new Input(new DataInputStream(y));
  }
 
  implicit def wrapOutput(out : DataOutput) : Output = new Output(out);
  implicit def wrapInput(out : DataInput) : Input = new Input(out);

  implicit def fileByName(name : String) : File = new File(name);

  def binary[T](implicit bin : Binary[T]) = bin;

  /**
   * Get the serialized value of this class as a byte array.
   */
  def toByteArray[T](t : T)(implicit bin : Binary[T]) : Array[Byte] = {
    val target = new ByteArrayOutputStream();
    wrapOutputStream(target).write(t);
    target.toByteArray(); 
  }
 
  /**
   * Read a value from the byte array. Anything past the end of the value will be
   * ignored.
   */ 
  def fromByteArray[T](array : Array[Byte])(implicit bin : Binary[T]) = wrapInputStream(new ByteArrayInputStream(array)).read[T];

  /** 
   * Convenience method for writing binary data to a file.
   */
  def toFile[T](t : T)(file : File)(implicit bin : Binary[T]) = {
    val raf = new RandomAccessFile(file, "rw");
    try{
      raf.write(toByteArray(t));}
    finally{
      raf.close(); }
  }

  /** 
   * Convenience method for reading binary data from a file.
   */
  def fromFile[T](file : File)(implicit bin : Binary[T]) = {
    val raf = new RandomAccessFile(file, "rw");
    try{
      val bytes =new Array[Byte](raf.length().intValue);
      raf.readFully(bytes);
      fromByteArray[T](bytes);}
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
    def reads(in : Input) = ((), 0);
    def writes(t : Unit)(out : Output) = ();
  }

  implicit object StringIsBinary extends Binary[String]{
    def reads(in : Input) = in.source.readUTF();
    def writes(t : String)(out : Output) = out.source.writeUTF(t); 
  }

  implicit object BooleanIsBinary extends Binary[Boolean]{
    def reads(in : Input) = in.source.readByte != 0
    def writes(t : Boolean)(out : Output) = out.write[Byte](if (t) (0x01) else (0x00));
  }

  implicit object ByteIsBinary extends Binary[Byte]{
    def reads(in : Input) = in.source.readByte()
    def writes(t : Byte)(out : Output) = out.source.writeByte(t);
  }

  implicit object CharIsBinary extends Binary[Char]{
    def reads(in : Input) = in.source.readChar()
    def writes(t : Char)(out : Output) = out.source.writeChar(t);
  }

  implicit object ShortIsBinary extends Binary[Short]{
    def reads(in : Input) = in.source.readShort()
    def writes(t : Short)(out : Output) = out.source.writeShort(t);
  }

  implicit object IntIsBinary extends Binary[Int]{
    def reads(in : Input) = in.source.readInt()
    def writes(t : Int)(out : Output) = out.source.writeInt(t);
    
  }

  implicit object LongIsBinary extends Binary[Long]{
    def reads(in : Input) = in.source.readLong();
    def writes(t : Long)(out : Output) = out.source.writeLong(t);
  }

  implicit object FloatIsBinary extends Binary[Float]{
    def reads(in : Input) = in.source.readFloat()
    def writes(t : Float)(out : Output) = out.source.writeFloat(t);
  }

  implicit object DoubleIsBinary extends Binary[Double]{
    def reads(in : Input) = in.source.readDouble()
    def writes(t : Double)(out : Output) = out.source.writeDouble(t);
  }

  implicit object BigIntIsBinary extends Binary[BigInt]{
    def reads(in : Input) = BigInt(in.read[Array[Byte]]);
    def writes(i : BigInt)(out : Output) = out.write(i.toByteArray);
  }

  implicit object BigDecimalIsBinary extends Binary[BigDecimal]{
    def reads(in : Input) = BigDecimal(in.read[String]);
    def writes(d : BigDecimal)(out : Output) = out.write(d.toString);
  }

  implicit object ClassIsBinary extends Binary[Class[T] forSome {type T;}]{
    def reads(in : Input) = Class.forName(in.read[String]);
    def writes(clazz : Class[T] forSome { type T; })(out : Output) = out.write(clazz.getName);
  }

  import sbinary.generic.Building._;
  import sbinary.generic.Generic._;

  implicit def listsAreBinary[T](implicit bin : Binary[T]) : Binary[List[T]] = lengthEncoded[T, List] 
  implicit def arraysAreBinary[T](implicit bin : Binary[T]) : Binary[Array[T]] = lengthEncoded[T, Array]
  implicit def immutableSetsAreBinary[T](implicit bin : Binary[T]) : Binary[immutable.Set[T]] = lengthEncoded[T, immutable.Set]
 
  implicit def immutableMapsAreBinary[S, T](implicit binS : Binary[S], binT : Binary[T]) : Binary[immutable.Map[S, T]] = new Binary[immutable.Map[S, T]]{
    def reads(in : Input) = immutable.Map.empty ++ in.read[Array[(S, T)]]
    def writes(ts : immutable.Map[S, T])(out : Output) = out.write(ts.toArray);
  }

  implicit def sortedMapsAreBinary[S, T](implicit ord : S => Ordered[S], binS : Binary[S], binT : Binary[T]) : Binary[immutable.SortedMap[S, T]] = new Binary[immutable.SortedMap[S, T]]{
    def reads(in : Input) = TreeMap[S, T](in.read[Array[(S, T)]] :_*)
    def writes(ts : immutable.SortedMap[S, T])(out : Output) = out.write(ts.toArray);
  }

  implicit def optionsAreBinary[S](implicit bin : Binary[S]) : Binary[Option[S]] = new Binary[Option[S]]{
    def reads(in : Input) = in.read[Byte] match {
      case 1 => Some(in.read[S]);
      case 0 => None
    }

    def writes(s : Option[S])(out : Output) = s match {
      case Some(x) => { out.write[Byte](0x1); out.write(x) }
      case None => out.write[Byte](0x0);
    }
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
      def reads (in : Input) : ${typeName} = ( 
    <#list 1..i as j>
        in.read[T${j}]<#if i!=j>,</#if>
    </#list>
      )
    
      def writes(tuple : ${typeName})(out : Output) = {
      <#list 1..i as j>
        out.write(tuple._${j});      
      </#list>;
      }
  }
</#list>

}
