package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import com.google.common.collect.Lists;

public class BaseMatcher<T> extends AbstractMatcher<T> {
  private Type type;
  private Matcher<T> parent;
  private List<Matcher<T>> list;

  public BaseMatcher() {

  }

  public BaseMatcher(Type type, Matcher<T> parent, List<Matcher<T>> list) {
    super();
    this.type = type;
    this.parent = parent;
    this.list = list;
  }

  @Override
  public boolean test(T t) {
    return type.apply(list.stream().map(p -> (Predicate<T>) p).collect(Collectors.toList()))
        .test(t);
  }

  @Override
  public Matcher<T> add(Predicate<T> predicate) {
    Matcher<T> matcher = new BaseMatcher<T>(Type.AND, this, Lists.newArrayList());
    list.add(matcher);
    return this;
  }
  
  @Override
  public Matcher<T> stratJunction(Type type) {
    Matcher<T> matcher = new BaseMatcher<T>(type, this, Lists.newArrayList());
    list.add(matcher);
    return matcher;
  }

  public Matcher<T> not(Predicate<T> other) {
    return stratJunction(Type.NOT);
  }

  @Override
  public Matcher<T> endJunction(Type type) {
    return Objects.equals(this.type, type) ? Optional.ofNullable(parent).orElse(this) : this;
  }
}
