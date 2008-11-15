package sbinary;

trait StandardPrimitives extends CoreProtocol{
  private def readUnsigned(in : Input) = readByte(in).toInt & 0xFF

  implicit object BooleanFormat extends Format[Boolean]{
    def reads(in : Input) = readByte(in) != 0
    def writes(out : Output, t : Boolean) = writeByte(out, if (t) (0x01) else (0x00));
  }

  implicit object CharFormat extends Format[Char]{
    def reads(in : Input) = ((readUnsigned(in) << 8) + readUnsigned(in)).toChar;
    def writes(out : Output, t : Char) = {
      writeByte(out, ((t >>> 8) & 0xFF).toByte);
      writeByte(out, ((t >>> 0) & 0xFF).toByte);
    }
  }

  implicit object ShortFormat extends Format[Short]{
    def reads(in : Input) = ((readUnsigned(in) << 8) + readUnsigned(in)).toShort

    def writes(out : Output, t : Short) = {
      writeByte(out, ((t >>> 8) & 0xFF).toByte);
      writeByte(out, t.toByte);
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
      writeByte(out, ((t >>> 24) & 0xFF).toByte);
      writeByte(out, ((t >>> 16) & 0xFF).toByte);
      writeByte(out, ((t >>>  8) & 0xFF).toByte);
      writeByte(out, ((t >>>  0) & 0xFF).toByte);
    } 
  }

  implicit object LongFormat extends Format[Long]{
    def reads(in : Input) = 
                (readUnsigned(in).toLong << 56) +
                (readUnsigned(in).toLong << 48) +
            		(readUnsigned(in) << 40) +
                (readUnsigned(in) << 32) +
                (readUnsigned(in) << 24) +
                (readUnsigned(in) << 16) +
                (readUnsigned(in) <<  8) +
                (readUnsigned(in) <<  0);
    def writes(out : Output, t : Long) = {
      writeByte(out, (t >>> 56).toByte);
      writeByte(out, (t >>> 48).toByte);
      writeByte(out, (t >>> 40).toByte);
      writeByte(out, (t >>> 32).toByte);
      writeByte(out, (t >>> 24).toByte);
      writeByte(out, (t >>> 16).toByte);
      writeByte(out, (t >>> 8).toByte);
      writeByte(out, (t >>> 0).toByte);
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
  private[this] def readUnsignedShort(in : Input) : Int = 
    (readByte(in).toInt << 8) + readByte(in)

  private[this] val buffers = new java.lang.ThreadLocal[(Array[Char], Array[Byte])]{
    override def initialValue() = (new Array[Char](80), new Array[Byte](80))
  }

  private[this] def fetchBuffers(size : Int) = {
    if (buffers.get()._1.length < size){
      buffers.set((new Array[Char](size * 2), new Array[Byte](size * 2)));
    }
    buffers.get();
  }

  implicit object StringFormat extends Format[String]{
    def reads(in : Input) = {
      val utflen = readUnsignedShort(in);
      val (cbuffer, bbuffer) = fetchBuffers(utflen);

      readFully(in, bbuffer, 0, utflen);

      var count, charCount, c, char2, char3 = 0;

      def malformed(index : Int) = error("Malformed input around byte " + index);
      def partial = error("Malformed input: Partial character at end");

      while(count < utflen){
        c = bbuffer(count).toInt & 0xFF
        (c >> 4) match {
          case 0|1|2|3|4|5|6|7 => {
            cbuffer(charCount) = c.toChar;
            count += 1;
          }
          case 12|13 => {
            count += 2;
            if (count > utflen) 
            char2 = bbuffer(count - 1).toInt;
            if ((char2 & 0xC0) != 0x80) malformed(count);
            cbuffer(charCount) = (((c & 0x1F) << 6) | (char2 & 0x3F)).toChar;
          }
          case 14 => {
            count += 3;
            char2 = bbuffer(count - 2);
            char3 = bbuffer(count - 1);
            if (((char2 & 0xC0) != 0x80) || ((char3 & 0xC0) != 0x80)) malformed(count - 1)
            cbuffer(charCount) = (((c     & 0x0F) << 12) |
                                  ((char2 & 0x3F) << 6)  |
                                  ((char3 & 0x3F) << 0)).toChar;
          }
          case _ => malformed(count);
        }
        charCount += 1;
      }

      new String(cbuffer, 0, charCount);
    }
 
    def writes(out : Output, value : String){
      var utflen = 0;
      
      {
        var i = 0;
        while(i < value.length){ 
          val c = value.charAt(i);
          utflen += (
            if ((c >= 0x0001) && (c <= 0x007F)) 1
	          else if (c > 0x07FF) 3
		        else 2) 
          i += 1;
        }
      }

      
	    if (utflen > 65535)
        error("encoded string too long: " + utflen + " bytes");

      val bbuffer = fetchBuffers(utflen + 2)._2;

      bbuffer(0) = ((utflen >>> 8) & 0xFF).toByte
      bbuffer(1) = (utflen & 0xFF).toByte

      var count = 2;
      
      var i = 0;
      def c = value.charAt(i);  
      def append(value : Int) {
        bbuffer(count) = value.toByte;
        count += 1;
      }

	    while((i < value.length) && !((c >= 0x0001) && (c <= 0x007F))) {
        i += 1;
        bbuffer(count) = c.toByte;
        count += 1; 
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

      writeAll(out, bbuffer, 0, utflen + 2); 
    }
  }  
}

trait JavaFormats extends StandardPrimitives with JavaUTF{
}

trait JavaIOProtocol extends Protocol with JavaIO{
  import java.io._;

  /**
   * Get the serialized value of this class as a byte array.
   */
  def toByteArray[T](t : T)(implicit bin : Writes[T]) : Array[Byte] = {
    val target = new ByteArrayOutputStream();
    write(target, t);
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
