package com.github.liudaomanbu.excel;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import com.google.common.collect.ImmutableList;

class ResultA {
  public static class Builder {
    public ResultB.Builder builder;

    public ResultA build() {
      return new ResultA(this);
    }
  }

  public final ResultB b;

  public ResultA(Builder builder) {
    builder.builder.a = this;
    b = builder.builder.build();
  }
}

class ResultB {
  public static class Builder {
    public ResultA a;

    public ResultB build() {
      return new ResultB(this);
    }
  }

  public final ResultA a;

  public ResultB(Builder builder) {
    this.a = builder.a;
  }
}


class Node {
  public static class Builder {
    public Node parent;
    public List<Node> childrens;
    public String value;

    public Node build() {
      return new Node(this);
    }
  }

  public final Node parent;
  public final ImmutableList<Node> childrens;
  public final String value;

  public Node(Builder builder) {
    this.parent = builder.parent;
    this.childrens = Optional.ofNullable(builder.childrens).map(Collection::stream)
        .orElse(Stream.empty()).collect(ImmutableList.toImmutableList());
    this.value = builder.value;
  }
}

public class TestBuilder {
  public static void main(String[] args) {
    testCyclicReference();
  }

  public static void testParent() {
    Node.Builder builder = new Node.Builder();
    builder.parent = builder.build();
    System.out.println();
  }

  public static void testCyclicReference() {
    ResultA.Builder builderA = new ResultA.Builder();
    builderA.builder = new ResultB.Builder();
    ResultA a = builderA.build();
    System.out.println(a);
    System.out.println(a.b);
  }
}
