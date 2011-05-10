package sbinary;

case class EOF() extends Exception;
object EOF{
  def eof = throw new EOF;
}

import EOF.eof

trait Input{

  /**
   * Read a byte. If we have come to the end of the stream, throws EOF.
   */
  def readByte : Byte;

  def readTo(target : Array[Byte], offset : Int, length : Int) : Int = {
    var i = 0;

    try{
      while (i < length){
        target(offset + i) = readByte;
        i += 1;
      }
    } catch {
      case EOF() =>;
    }

    i;
  }

  def readTo(target : Array[Byte]) : Int = readTo(target, 0, target.length);

  def readFully(target : Array[Byte], offset : Int, length : Int){
    var bytesRead = 0;

    while(bytesRead < length){
      val newRead = readTo(target, offset + bytesRead, length - bytesRead);
      if (newRead <= 0) eof;
      bytesRead += newRead;
    }
  }

  def readFully(target : Array[Byte]){
    readFully(target, 0, target.length);
  } 
}

trait Output{
  def writeByte(value : Byte);

  def writeAll(source : Array[Byte], offset : Int, length : Int) {
    var i = 0;

    while(i < length){
      writeByte(source(offset + i));
      i += 1;
    }
  }

  def writeAll(source : Array[Byte]){
    writeAll(source, 0, source.length);
  }
}

import java.io._;

class JavaInput(in : InputStream) extends Input{
  def readByte = in.read() match {
    case x if x < 0 => eof; 
    case x => x.toByte;
  }

  override def readTo(target : Array[Byte], offset : Int, length : Int) : Int = 
    in.read(target, offset, length) match {
      case x if x < 0 => eof;
      case x => x;
    }
}

class JavaOutput(out : OutputStream) extends Output{
  def writeByte(value : Byte) = out.write(value);
  override def writeAll(source : Array[Byte], offset : Int, length : Int) = out.write(source, offset, length);
}

object Input {
  implicit def javaInputToInput(x : InputStream): JavaInput = new JavaInput(x)
}
object Output {
  implicit def javaOutputToOutput(x : OutputStream): JavaOutput = new JavaOutput(x);
}
