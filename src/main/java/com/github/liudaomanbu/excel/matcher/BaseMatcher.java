package com.github.liudaomanbu.excel.matcher;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import com.github.liudaomanbu.excel.matcher.constant.ComparableMatcherType;
import com.github.liudaomanbu.excel.matcher.constant.StringMatcherType;
import com.github.liudaomanbu.excel.matcher.constant.Type;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

public class BaseMatcher<T> implements Matcher<T> {
  public static final Type DEFAULT_TYPE = Type.AND;
  public static final String SCRIPT_VALUE_KEY = "value";

  private Type type;
  private Matcher<T> parent;
  private List<Predicate<T>> predicates;

  public BaseMatcher() {
    this(null, null, null);
  }

  public BaseMatcher(Type type, Matcher<T> parent, List<Predicate<T>> predicates) {
    super();
    this.type = Optional.ofNullable(type).orElse(DEFAULT_TYPE);
    this.parent = parent;
    this.predicates = Optional.ofNullable(predicates).orElse(Lists.newLinkedList());
  }

  @Override
  public boolean test(T t) {
    return type.reduce(predicates).test(t);
  }

  @Override
  public Matcher<T> add(Predicate<T> predicate) {
    predicates.add(predicate);
    return this;
  }

  @Override
  public <R> Matcher<T> add(Predicate<R> predicate, Function<T, R> transformer) {
    return add(value -> predicate.test(transformer.apply(value)));
  }

  @Override
  public Matcher<T> add(StringMatcherType type, String predicateValue,
      Function<T, String> transformer) {
    return add(value -> type.apply(value, predicateValue), transformer);
  }

  @Override
  public <R extends Comparable<R>> Matcher<T> add(ComparableMatcherType type, R predicateValue,
      Function<T,? extends R> transformer) {
    return add(value -> type.apply(value, predicateValue), transformer);
  }

  public Matcher<T> stratJunction(Type type) {
    Matcher<T> builder = new BaseMatcher<>(type, null, null);
    add(builder);
    return builder;
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

  @Override
  public Predicate<T> reduce() {
    Preconditions.checkState(!predicates.isEmpty(),"predicates can't empty");
    return type.reduce(predicates);
  }

  public Type getType() {
    return type;
  }

  public Matcher<T> setType(Type type) {
    this.type = type;
    return this;
  }

  public Matcher<T> getParent() {
    return parent;
  }

  public Matcher<T> setParent(Matcher<T> parent) {
    this.parent = parent;
    return this;
  }

  public List<Predicate<T>> getPredicates() {
    return predicates;
  }

  public Matcher<T> setPredicates(List<Predicate<T>> predicates) {
    this.predicates = predicates;
    return this;
  }

}
