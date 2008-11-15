package sbinary;

trait Protocol extends IO{
  trait Reads[T]{
    def reads(in : Input) : T;
  }

  trait Writes[T]{
    def writes(out : Output, value : T) : Unit;
  }

  trait Format[T] extends Reads[T] with Writes[T];

  implicit object ByteFormat extends Format[Byte]{
    def reads(in : Input) = readByte(in);
    def writes(out : Output, value : Byte) = writeByte(out, value); 
  }

  def format[T](implicit fm : Format[T]) = fm;
}

trait CoreProtocol extends Protocol{
  implicit val IntFormat : Format[Int];
  implicit val ShortFormat : Format[Short];
  implicit val LongFormat : Format[Long];
  implicit val BooleanFormat : Format[Boolean];
  implicit val CharFormat : Format[Char];
  implicit val FloatFormat : Format[Float];
  implicit val DoubleFormat : Format[Double];
  implicit val StringFormat : Format[String];
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

trait JavaFormats{



}
