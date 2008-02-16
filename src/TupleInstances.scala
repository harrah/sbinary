package sbinary;
import java.io._;
import Operations._;

object TupleInstances{

<#list 2..22 as i>
  <#assign typeName>
   Tuple${i}[<#list 1..i as j>T${j} <#if i != j>,</#if></#list>]
  </#assign>
  implicit def tuple${i}IsBinary[<#list 1..i as j>T${j}<#if i !=j>,</#if></#list>](implicit 
    <#list 1..i as j>
      bin${j} : Binary[T${j}] <#if i != j>,</#if>
    </#list>
    ) : Binary[${typeName}] = new Binary[${typeName}]{
      def readsWithSize (input : DataInput) : (${typeName}, Int) = {
    <#list 1..i as j>
        val (value${j}, size${j}) = readWithSize[T${j}](input);
    </#list>
      ((<#list 1..i as j>value${j}<#if i!=j>,</#if></#list>),
      <#list 1..i as j>size${j}<#if i!=j>+</#if></#list>)
    }

      def writes(tuple : ${typeName})(output : DataOutput) = 
      <#list 1..i as j>write(tuple._${j})(output) <#if i !=j> + </#if>
      </#list>;
  }
</#list>
}
