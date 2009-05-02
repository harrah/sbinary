package sbinary;

import Operations._;

trait StandardPrimitives extends CoreProtocol{
  private def readUnsigned(in : Input) = in.readByte.toInt & 0xFF

  implicit object BooleanFormat extends Format[Boolean]{
    def reads(in : Input) = in.readByte != 0
    def writes(out : Output, t : Boolean) = out.writeByte(if (t) (0x01) else (0x00));
  }

  implicit object CharFormat extends Format[Char]{
    def reads(in : Input) = ((readUnsigned(in) << 8) + readUnsigned(in)).toChar;
    def writes(out : Output, t : Char) = {
      out.writeByte(((t >>> 8) & 0xFF).toByte);
      out.writeByte(((t >>> 0) & 0xFF).toByte);
    }
  }

  implicit object ShortFormat extends Format[Short]{
    def reads(in : Input) = ((readUnsigned(in) << 8) + readUnsigned(in)).toShort

    def writes(out : Output, t : Short) = {
      out.writeByte(((t >>> 8) & 0xFF).toByte);
      out.writeByte(t.toByte);
    } 
  }

  implicit object IntFormat extends Format[Int]{
    def reads(in : Input) = {
      val ch1 = readUnsigned(in);
      val ch2 = readUnsigned(in);
      val ch3 = readUnsigned(in);
      val ch4 = readUnsigned(in);
      ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0)) 
    }

    def writes(out : Output, t : Int) {
      out.writeByte(((t >>> 24) & 0xFF).toByte);
      out.writeByte(((t >>> 16) & 0xFF).toByte);
      out.writeByte(((t >>>  8) & 0xFF).toByte);
      out.writeByte(((t >>>  0) & 0xFF).toByte);
    } 
  }

  implicit object LongFormat extends Format[Long]{
    def reads(in : Input) = 
                (readUnsigned(in).toLong << 56) +
                (readUnsigned(in).toLong << 48) +
            		(readUnsigned(in).toLong << 40) +
                (readUnsigned(in).toLong << 32) +
                (readUnsigned(in).toLong << 24) +
                (readUnsigned(in).toLong << 16) +
                (readUnsigned(in).toLong <<  8) +
                (readUnsigned(in).toLong <<  0);
    def writes(out : Output, t : Long) = {
      out.writeByte((t >>> 56).toByte);
      out.writeByte((t >>> 48).toByte);
      out.writeByte((t >>> 40).toByte);
      out.writeByte((t >>> 32).toByte);
      out.writeByte((t >>> 24).toByte);
      out.writeByte((t >>> 16).toByte);
      out.writeByte((t >>> 8).toByte);
      out.writeByte((t >>> 0).toByte);
    }
  }

  implicit object FloatFormat extends Format[Float]{
    def reads(in : Input) = java.lang.Float.intBitsToFloat(read[Int](in))
    def writes(out : Output, t : Float) = write[Int](out, java.lang.Float.floatToIntBits(t));
  }

  implicit object DoubleFormat extends Format[Double]{
    def reads(in : Input) = java.lang.Double.longBitsToDouble(read[Long](in));
    def writes(out : Output, t : Double) = write[Long](out, java.lang.Double.doubleToLongBits(t));
  }
}

trait JavaUTF extends CoreProtocol{
  private[this] def readUnsignedByte(in : Input) : Int = in.readByte.toInt & 0xFF
  private[this] def readUnsignedShort(in : Input) : Int = 
    (readUnsignedByte(in) << 8) + readUnsignedByte(in)

  private[this] val buffers = new java.lang.ThreadLocal[(Array[Char], Array[Byte])]{
    override def initialValue() = (new Array[Char](80), new Array[Byte](80))
  }

  private[this] def fetchBuffers(size : Int) = {
    if (buffers.get()._1.length < size){
      buffers.set((new Array[Char](size * 2), new Array[Byte](size * 2)));
    }
    buffers.get();
  }

  /**
   * A Format for strings compatible with the modified UTF format used by 
   * java.io.DataInput and java.io.DataOutput.
   */
  implicit object StringFormat extends Format[String]{
    def reads(in : Input) = {
      val utflen = readUnsignedShort(in);
      val (cbuffer, bbuffer) = fetchBuffers(utflen);

      in.readFully(bbuffer, 0, utflen);

      var count, charCount, c, char2, char3 = 0;

      def malformed(index : Int) = error("Malformed input around byte " + index);
      def partial = error("Malformed input: Partial character at end");

      while((count < utflen) && {c = bbuffer(count) & 0xff; c <= 127 }) {
        cbuffer(charCount) = c.toChar;
        charCount += 1;
        count += 1;
      }

      while(count < utflen){
        c = bbuffer(count).toInt & 0xFF
        cbuffer(charCount) = ((c >> 4) match {
          case 0|1|2|3|4|5|6|7 => {
            count += 1;
            c
          }
          case 12|13 => {
            count += 2;
            if (count > utflen) partial;

            char2 = bbuffer(count - 1)
            if ((char2 & 0xC0) != 0x80) malformed(count);
            (((c & 0x1F) << 6) | (char2 & 0x3F));
          }
          case 14 => {
            count += 3;
            char2 = bbuffer(count - 2);
            char3 = bbuffer(count - 1);
            if (((char2 & 0xC0) != 0x80) || ((char3 & 0xC0) != 0x80)) 
              malformed(count - 1);

            ((c & 0x0F).toInt << 12) | ((char2 & 0x3F).toInt << 6)  | ((char3 & 0x3F).toInt << 0);
          }
          case _ => malformed(count);
        }).toChar
        charCount += 1;
      }

      new String(cbuffer, 0, charCount);
    }
 
    def writes(out : Output, value : String){
      var utflen = 0;
      
      var i = 0;
      def c = value.charAt(i);  

      while(i < value.length){ 
        utflen += (
          if ((c >= 0x0001) && (c <= 0x007F)) 1
          else if (c > 0x07FF) 3
          else 2) 
        i += 1;
      }
     
	    if (utflen > 65535)
        error("encoded string too long: " + utflen + " bytes");

      val bbuffer = fetchBuffers(utflen + 2)._2;
      var count = 0;
      def append(value : Int) {
        bbuffer(count) = value.toByte;
        count += 1;
      }

      append((utflen >>> 8) & 0xFF)
      append(utflen & 0xFF)

      i = 0;
	    while((i < value.length) && ((c >= 0x0001) && (c <= 0x007F))) {
        bbuffer(count) = c.toByte;
        count += 1; 
        i += 1;
      }
 
      while(i < value.length){
        if ((c >= 0x0001) && (c <= 0x007F)) {
          append(c);
        } else if (c > 0x07FF) {
          append(0xE0 | ((c >> 12) & 0x0F));
          append(0x80 | ((c >>  6) & 0x3F));
          append(0x80 | ((c >>  0) & 0x3F));
        } else {
          append(0xC0 | ((c >>  6) & 0x1F));
          append(0x80 | ((c >>  0) & 0x3F));
        }

        i += 1;
      }

      out.writeAll(bbuffer, 0, utflen + 2); 
    }
  }  
}

trait JavaFormats extends StandardPrimitives with JavaUTF{
}

trait JavaIOProtocol extends Protocol{
  import java.io._;
  import JavaIO._;

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
