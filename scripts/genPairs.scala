println("package binary;");
println("import java.io._;");
println("import Operations._;");

println ("object TupleInstances{");

for(i <- 2 to 22){
  val name = "Tuple" + i;
  val typeParams = for (j <- 1 to i) yield "T" + j;
  val typeParamString = typeParams.mkString(",");
  val typeString = name + "[" + typeParamString + "]";
  print ("  implicit def tuple" + i + "IsBinary[");
  print (typeParamString);
  print("]");
  println("(implicit\n")
  print((1 to i).map(j => "    bin" + j + " : Binary[T" + j + "]").mkString(",\n"));
  println(")");
  val binaryType = "Binary[" + typeString + "]";
  println(" : " + binaryType + " = new " + binaryType + "{");
  println ("    def reads(stream : DataInputStream) : " + typeString +  "  = ("); 
  print ( typeParams.map(s => "    read[" + s + "](stream)").mkString(",\n"))
  println("  )");
  println;

  println ("    def writes( ts : " + typeString + ")(stream : DataOutputStream) = {");

  for (j <- 1 to i){
    println ( "     write(ts._" + j + ")(stream);");    
  }
  println("    }");
  println("  }");
}

println("}");
