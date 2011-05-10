
package sbinary;

object Operations{
  import java.io.{BufferedInputStream, BufferedOutputStream, ByteArrayInputStream, ByteArrayOutputStream, File, FileInputStream, FileOutputStream};

  def format[T](implicit fm : Format[T]) = fm;

  def read[T](in : Input)(implicit reader : Reads[T]) = reader.reads(in);
  def write[T](out : Output, value : T)(implicit writer : Writes[T]) = writer.writes(out, value);


  /**
   * Get the serialized value of this class as a byte array.
   */
  def toByteArray[T](t : T)(implicit bin : Writes[T]) : Array[Byte] = {
    val target = new ByteArrayOutputStream();
    bin.writes(target, t);
    target.toByteArray(); 
  }
 
  /**
   * Read a value from the byte array. Anything past the end of the value will be
   * ignored.
   */ 
  def fromByteArray[T](array : Array[Byte])(implicit bin : Reads[T]) = read[T](new ByteArrayInputStream(array));

  /** 
   * Convenience method for writing binary data to a file.
   */
  def toFile[T](t : T)(file : File)(implicit bin : Writes[T]) = {
    val out = new BufferedOutputStream(new FileOutputStream(file));
    try{
      out.write(toByteArray(t));}
    finally{
      out.close(); }
  }

  /** 
   * Convenience method for reading binary data from a file.
   */
  def fromFile[T](file : File)(implicit bin : Reads[T]) = {
    val in = new BufferedInputStream(new FileInputStream(file))
    try{
      read[T](in)
    } finally{
      in.close(); 
    }
  }
}
