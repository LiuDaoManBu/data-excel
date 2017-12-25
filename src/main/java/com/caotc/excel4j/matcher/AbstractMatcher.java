package com.caotc.excel4j.matcher;

public abstract class AbstractMatcher<T> implements Matcher<T> {
  public abstract Matcher<T> stratJunction(Type type);

  public abstract Matcher<T> endJunction(Type type);

  @Override
  public Matcher<T> not() {
    return stratJunction(Type.NOT);
  }

  @Override
  public Matcher<T> and() {
    return stratJunction(Type.AND);
  }

  @Override
  public Matcher<T> or() {
    return stratJunction(Type.OR);
  }

  @Override
  public Matcher<T> endNot() {
    return endJunction(Type.NOT);
  }

  @Override
  public Matcher<T> endAnd() {
    return endJunction(Type.AND);
  }

  @Override
  public Matcher<T> endOr() {
    return endJunction(Type.OR);
  }

}
