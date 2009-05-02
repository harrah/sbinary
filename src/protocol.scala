package sbinary;

trait Protocol{
  trait Reads[T]{
    def reads(in : Input) : T;
  }

  trait Writes[T]{
    def writes(out : Output, value : T) : Unit;
  }

  trait Format[T] extends Reads[T] with Writes[T];

  implicit object ByteFormat extends Format[Byte]{
    def reads(in : Input) = in.readByte;
    def writes(out : Output, value : Byte) = out.writeByte(value); 
  }

  implicit object UnitFormat extends Format[Unit]{
    def reads(in : Input){}
    def writes(out : Output, value : Unit) {}
  }

  def format[T](implicit fm : Format[T]) = fm;

  def read[T](in : Input)(implicit reader : Reads[T]) = reader.reads(in);
  def write[T](out : Output, value : T)(implicit writer : Writes[T]) = writer.writes(out, value);


  /**
   * Returns an iterator that iterates by reading from this input.
   * In order to ensure proper laziness properties (and not reading more
   * data than is strictly neccessary) this will always return true 
   * from hasNext but may throw an EOFException on an unexpected end
   * of stream.
   */
  def asIterator[S](input : Input)(implicit bin : Reads[S]) = new Iterator[S]{
    def hasNext = true;
    def next = read[S](input);
  }
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


