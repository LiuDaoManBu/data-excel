package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.constant.Type;
import com.google.common.collect.Lists;

public class BaseMatcher<T> implements Matcher<T> {
  private final Type type;
  private final Matcher<T> parent;
  private final List<Predicate<T>> list;

  public BaseMatcher() {
    this(Type.AND);
  }

  public BaseMatcher(Type type) {
    this(type, null);
  }

  public BaseMatcher(Type type, Matcher<T> parent) {
    this(type, parent, Lists.newArrayList());
  }

  public BaseMatcher(Type type, Matcher<T> parent, List<Predicate<T>> list) {
    super();
    this.type = type;
    this.parent = parent;
    this.list = list;
  }

  @Override
  public boolean test(T t) {
    return type.apply(list).test(t);
  }

  @Override
  public Matcher<T> add(Predicate<T> predicate) {
    list.add(predicate);
    return this;
  }

  @Override
  public <R> Matcher<T> add(Predicate<R> predicate, Function<T, R> transform) {
    return add(value -> predicate.test(transform.apply(value)));
  }

  @Override
  public Matcher<T> add(StringMatcherType type, String predicateValue,
      Function<T, String> transform) {
    return add(value -> type.apply(value, predicateValue), transform);
  }

  @Override
  public <R extends Comparable<R>> Matcher<T> add(ComparableMatcherType type, R predicateValue,
      Function<T, R> transform) {
    return add(value -> type.apply(value, predicateValue), transform);
  }

  public Matcher<T> stratJunction(Type type) {
    Matcher<T> matcher = new BaseMatcher<T>(type, this);
    add(matcher);
    return matcher;
  }

  public Matcher<T> endJunction(Type type) {
    return Objects.equals(this.type, type) ? Optional.ofNullable(parent).orElse(this) : this;
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
  public Matcher<T> endAnd() {
    return endJunction(Type.AND);
  }

  @Override
  public Matcher<T> endOr() {
    return endJunction(Type.OR);
  }

}
