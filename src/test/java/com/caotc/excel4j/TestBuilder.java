package com.caotc.excel4j;


class ResultA{
  public static class Builder{
    public ResultB.Builder builder;
    public ResultA build() {
      return new ResultA(this);
    }
  }
  public final ResultB b;
  public ResultA(Builder builder) {
    builder.builder.a=this;
    b=builder.builder.build();
  }
}

class ResultB{
  public static class Builder{
    public ResultA a;
    public ResultB build() {
      return new ResultB(this);
    }
  }
  public final ResultA a;
  public ResultB(Builder builder) {
    this.a=builder.a;
  }
}

public class TestBuilder {
  public static void main(String[] args) {
    testCyclicReference();
  }
  
  public static void testCyclicReference() {
    ResultA.Builder builderA=new ResultA.Builder();
    builderA.builder=new ResultB.Builder();
    ResultA a=builderA.build();
    System.out.println(a);
    System.out.println(a.b);
  }
}
