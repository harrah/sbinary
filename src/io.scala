package sbinary;

case object EOF extends Throwable{
  override def fillInStackTrace = this;
}

trait IO{
  type Input;
  type Output;

  def eof = throw EOF;

  /**
   * Read a byte. If we have come to the end of the stream, throws EOF.
   */
  def readByte(in : Input) : Byte;

  def readTo(in : Input, target : Array[Byte], offset : Int, length : Int) : Int = {
    var i = 0;

    try{
      while (i < length){
        target(offset + i) = readByte(in);
        i += 1;
      }
    } catch {
      case EOF =>;
    }

    i;
  }

  def readTo(in : Input, target : Array[Byte]) : Int = readTo(in, target, 0, target.length);

  def readFully(in : Input, target : Array[Byte], offset : Int, length : Int){
    var bytesRead = 0;

    while(bytesRead < length){
      val newRead = readTo(in, target, offset + bytesRead, length - bytesRead);
      if (newRead <= 0) eof;
      bytesRead += newRead;
    }
  }

  def readFully(in : Input, target : Array[Byte]){
    readFully(in, target, 0, target.length);
  } 

  def writeByte(out : Output, value : Byte);

  def writeAll(out : Output, source : Array[Byte], offset : Int, length : Int) {
    var i = 0;

    while(i < length){
      writeByte(out, source(offset + i));
      i += 1;
    }
  }

  def writeAll(out : Output, source : Array[Byte]){
    writeAll(out, source, 0, source.length);
  }

  def flush(out : Output){ }
}

trait JavaIO extends IO{
  import java.io._;

  type Input = java.io.InputStream;
  type Output = java.io.OutputStream;

  def readByte(in : Input) = in.read() match {
    case x if x < 0 => eof; 
    case x => x.toByte;
  }

  override def readTo(in : Input, target : Array[Byte], offset : Int, length : Int) : Int = 
    in.read(target, offset, length) match {
      case x if x < 0 => eof;
      case x => x;
    }

  def writeByte(out : Output, value : Byte) = out.write(value);
  override def writeAll(out : Output, source : Array[Byte], offset : Int, length : Int) = out.write(source, offset, length);
}
