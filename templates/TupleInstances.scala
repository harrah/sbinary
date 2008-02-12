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
      def reads (input : DataInput) = (
    <#list 1..i as j>
        read[T${j}](input) <#if i != j>,</#if>
    </#list>)

      def writes(tuple : ${typeName})(output : DataOutput) = 
      <#list 1..i as j>write(tuple._${j})(output) <#if i !=j> + </#if>
      </#list>;
      
    }
</#list>
}
