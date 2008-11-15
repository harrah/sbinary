package sbinary;

trait StandardTypes extends Generic{
  implicit def listFormat[T](implicit bin : Format[T]) : Format[List[T]] = 
    new LengthEncoded[List[T], T]{
      def build(length : Int, ts : Iterator[T]) = {
        val buffer = new scala.collection.mutable.ListBuffer[T];
        ts.foreach(buffer += (_ : T));
        buffer.toList;
      } 
    }

  implicit def arrayFormat[T](implicit fmt : Format[T]) : Format[Array[T]] = fmt match{
    case ByteFormat => ByteArrayFormat.asInstanceOf[Format[Array[T]]];
    case _ => 
      new LengthEncoded[Array[T], T]{
        def build(length : Int, ts : Iterator[T]) = {
          val result = new Array[T](length);
          ts.copyToArray(result, 0);
          result;
        }
      } 
    }

  implicit object ByteArrayFormat extends Format[Array[Byte]]{
    def reads(in : Input) = {
      val length = read[Int](in);
      val bytes = new Array[Byte](length);
      readFully(in, bytes);
      bytes; 
    }

    def writes(out : Output, bytes : Array[Byte]){
      write(out, bytes.length);
      writeAll(out, bytes);
    }
  }

  implicit object BigIntFormat extends Format[BigInt]{
    def reads(in : Input) = BigInt(read[Array[Byte]](in));
    def writes(out : Output, i : BigInt) = write(out, i.toByteArray);
  }

  implicit object BigDecimalFormat extends Format[BigDecimal]{
    def reads(in : Input) = BigDecimal(read[String](in));
    def writes(out : Output, d : BigDecimal) = write(out, d.toString);
  }

  implicit object ClassFormat extends Format[Class[_]]{
    def reads(in : Input) = Class.forName(read[String](in));
    def writes(out : Output, clazz : Class[_]) = write(out, clazz.getName);
  }

  implicit val SymbolFormat : Format[Symbol] = viaString(Symbol(_));

  import java.io.File;
  implicit val FileFormat : Format[File] = viaString(new File(_ : String));

  import java.net.{URI, URL}
  implicit val UrlFormat : Format[URL] = viaString(new URL(_ : String));
  implicit val UriFormat : Format[URI] = viaString(new URI(_ : String));


  import scala.xml.{XML, Elem, NodeSeq};
  implicit val XmlFormat : Format[NodeSeq] = new Format[NodeSeq]{
    def reads(in : Input) = XML.loadString(read[String](in)).child;
    def writes(out : Output, elem : NodeSeq) = write(out, <binary>elem</binary>.toString);
  }
}
