package binary;
import java.io._;
import Operations._;
object TupleInstances{
  implicit def tuple2IsBinary[T1,T2](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2])
 : Binary[Tuple2[T1,T2]] = new Binary[Tuple2[T1,T2]]{
    def reads(stream : DataInputStream) : Tuple2[T1,T2]  = (
    read[T1](stream),
    read[T2](stream)  )

    def writes( ts : Tuple2[T1,T2])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
    }
  }
  implicit def tuple3IsBinary[T1,T2,T3](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3])
 : Binary[Tuple3[T1,T2,T3]] = new Binary[Tuple3[T1,T2,T3]]{
    def reads(stream : DataInputStream) : Tuple3[T1,T2,T3]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream)  )

    def writes( ts : Tuple3[T1,T2,T3])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
    }
  }
  implicit def tuple4IsBinary[T1,T2,T3,T4](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4])
 : Binary[Tuple4[T1,T2,T3,T4]] = new Binary[Tuple4[T1,T2,T3,T4]]{
    def reads(stream : DataInputStream) : Tuple4[T1,T2,T3,T4]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream)  )

    def writes( ts : Tuple4[T1,T2,T3,T4])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
    }
  }
  implicit def tuple5IsBinary[T1,T2,T3,T4,T5](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5])
 : Binary[Tuple5[T1,T2,T3,T4,T5]] = new Binary[Tuple5[T1,T2,T3,T4,T5]]{
    def reads(stream : DataInputStream) : Tuple5[T1,T2,T3,T4,T5]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream)  )

    def writes( ts : Tuple5[T1,T2,T3,T4,T5])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
    }
  }
  implicit def tuple6IsBinary[T1,T2,T3,T4,T5,T6](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6])
 : Binary[Tuple6[T1,T2,T3,T4,T5,T6]] = new Binary[Tuple6[T1,T2,T3,T4,T5,T6]]{
    def reads(stream : DataInputStream) : Tuple6[T1,T2,T3,T4,T5,T6]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream)  )

    def writes( ts : Tuple6[T1,T2,T3,T4,T5,T6])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
    }
  }
  implicit def tuple7IsBinary[T1,T2,T3,T4,T5,T6,T7](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7])
 : Binary[Tuple7[T1,T2,T3,T4,T5,T6,T7]] = new Binary[Tuple7[T1,T2,T3,T4,T5,T6,T7]]{
    def reads(stream : DataInputStream) : Tuple7[T1,T2,T3,T4,T5,T6,T7]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream)  )

    def writes( ts : Tuple7[T1,T2,T3,T4,T5,T6,T7])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
    }
  }
  implicit def tuple8IsBinary[T1,T2,T3,T4,T5,T6,T7,T8](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8])
 : Binary[Tuple8[T1,T2,T3,T4,T5,T6,T7,T8]] = new Binary[Tuple8[T1,T2,T3,T4,T5,T6,T7,T8]]{
    def reads(stream : DataInputStream) : Tuple8[T1,T2,T3,T4,T5,T6,T7,T8]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream)  )

    def writes( ts : Tuple8[T1,T2,T3,T4,T5,T6,T7,T8])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
    }
  }
  implicit def tuple9IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9])
 : Binary[Tuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9]] = new Binary[Tuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9]]{
    def reads(stream : DataInputStream) : Tuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream)  )

    def writes( ts : Tuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
    }
  }
  implicit def tuple10IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10])
 : Binary[Tuple10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]] = new Binary[Tuple10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]]{
    def reads(stream : DataInputStream) : Tuple10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream)  )

    def writes( ts : Tuple10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
    }
  }
  implicit def tuple11IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11])
 : Binary[Tuple11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]] = new Binary[Tuple11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]]{
    def reads(stream : DataInputStream) : Tuple11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream)  )

    def writes( ts : Tuple11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
    }
  }
  implicit def tuple12IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12])
 : Binary[Tuple12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]] = new Binary[Tuple12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]]{
    def reads(stream : DataInputStream) : Tuple12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream)  )

    def writes( ts : Tuple12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
    }
  }
  implicit def tuple13IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13])
 : Binary[Tuple13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]] = new Binary[Tuple13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]]{
    def reads(stream : DataInputStream) : Tuple13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream)  )

    def writes( ts : Tuple13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
    }
  }
  implicit def tuple14IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14])
 : Binary[Tuple14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]] = new Binary[Tuple14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]]{
    def reads(stream : DataInputStream) : Tuple14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream)  )

    def writes( ts : Tuple14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
    }
  }
  implicit def tuple15IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15])
 : Binary[Tuple15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]] = new Binary[Tuple15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]]{
    def reads(stream : DataInputStream) : Tuple15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream)  )

    def writes( ts : Tuple15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
    }
  }
  implicit def tuple16IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16])
 : Binary[Tuple16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]] = new Binary[Tuple16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]]{
    def reads(stream : DataInputStream) : Tuple16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream)  )

    def writes( ts : Tuple16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
    }
  }
  implicit def tuple17IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16],
    bin17 : Binary[T17])
 : Binary[Tuple17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]] = new Binary[Tuple17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]]{
    def reads(stream : DataInputStream) : Tuple17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream),
    read[T17](stream)  )

    def writes( ts : Tuple17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
     write(ts._17)(stream);
    }
  }
  implicit def tuple18IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16],
    bin17 : Binary[T17],
    bin18 : Binary[T18])
 : Binary[Tuple18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]] = new Binary[Tuple18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]]{
    def reads(stream : DataInputStream) : Tuple18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream),
    read[T17](stream),
    read[T18](stream)  )

    def writes( ts : Tuple18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
     write(ts._17)(stream);
     write(ts._18)(stream);
    }
  }
  implicit def tuple19IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16],
    bin17 : Binary[T17],
    bin18 : Binary[T18],
    bin19 : Binary[T19])
 : Binary[Tuple19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]] = new Binary[Tuple19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]]{
    def reads(stream : DataInputStream) : Tuple19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream),
    read[T17](stream),
    read[T18](stream),
    read[T19](stream)  )

    def writes( ts : Tuple19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
     write(ts._17)(stream);
     write(ts._18)(stream);
     write(ts._19)(stream);
    }
  }
  implicit def tuple20IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16],
    bin17 : Binary[T17],
    bin18 : Binary[T18],
    bin19 : Binary[T19],
    bin20 : Binary[T20])
 : Binary[Tuple20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]] = new Binary[Tuple20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]]{
    def reads(stream : DataInputStream) : Tuple20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream),
    read[T17](stream),
    read[T18](stream),
    read[T19](stream),
    read[T20](stream)  )

    def writes( ts : Tuple20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
     write(ts._17)(stream);
     write(ts._18)(stream);
     write(ts._19)(stream);
     write(ts._20)(stream);
    }
  }
  implicit def tuple21IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16],
    bin17 : Binary[T17],
    bin18 : Binary[T18],
    bin19 : Binary[T19],
    bin20 : Binary[T20],
    bin21 : Binary[T21])
 : Binary[Tuple21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]] = new Binary[Tuple21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]]{
    def reads(stream : DataInputStream) : Tuple21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream),
    read[T17](stream),
    read[T18](stream),
    read[T19](stream),
    read[T20](stream),
    read[T21](stream)  )

    def writes( ts : Tuple21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
     write(ts._17)(stream);
     write(ts._18)(stream);
     write(ts._19)(stream);
     write(ts._20)(stream);
     write(ts._21)(stream);
    }
  }
  implicit def tuple22IsBinary[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](implicit

    bin1 : Binary[T1],
    bin2 : Binary[T2],
    bin3 : Binary[T3],
    bin4 : Binary[T4],
    bin5 : Binary[T5],
    bin6 : Binary[T6],
    bin7 : Binary[T7],
    bin8 : Binary[T8],
    bin9 : Binary[T9],
    bin10 : Binary[T10],
    bin11 : Binary[T11],
    bin12 : Binary[T12],
    bin13 : Binary[T13],
    bin14 : Binary[T14],
    bin15 : Binary[T15],
    bin16 : Binary[T16],
    bin17 : Binary[T17],
    bin18 : Binary[T18],
    bin19 : Binary[T19],
    bin20 : Binary[T20],
    bin21 : Binary[T21],
    bin22 : Binary[T22])
 : Binary[Tuple22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]] = new Binary[Tuple22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]]{
    def reads(stream : DataInputStream) : Tuple22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]  = (
    read[T1](stream),
    read[T2](stream),
    read[T3](stream),
    read[T4](stream),
    read[T5](stream),
    read[T6](stream),
    read[T7](stream),
    read[T8](stream),
    read[T9](stream),
    read[T10](stream),
    read[T11](stream),
    read[T12](stream),
    read[T13](stream),
    read[T14](stream),
    read[T15](stream),
    read[T16](stream),
    read[T17](stream),
    read[T18](stream),
    read[T19](stream),
    read[T20](stream),
    read[T21](stream),
    read[T22](stream)  )

    def writes( ts : Tuple22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22])(stream : DataOutputStream) = {
     write(ts._1)(stream);
     write(ts._2)(stream);
     write(ts._3)(stream);
     write(ts._4)(stream);
     write(ts._5)(stream);
     write(ts._6)(stream);
     write(ts._7)(stream);
     write(ts._8)(stream);
     write(ts._9)(stream);
     write(ts._10)(stream);
     write(ts._11)(stream);
     write(ts._12)(stream);
     write(ts._13)(stream);
     write(ts._14)(stream);
     write(ts._15)(stream);
     write(ts._16)(stream);
     write(ts._17)(stream);
     write(ts._18)(stream);
     write(ts._19)(stream);
     write(ts._20)(stream);
     write(ts._21)(stream);
     write(ts._22)(stream);
    }
  }
}
