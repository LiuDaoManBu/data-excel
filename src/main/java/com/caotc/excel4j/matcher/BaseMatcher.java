package com.caotc.excel4j.matcher;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import com.google.common.collect.Lists;

public class BaseMatcher<T> implements Matcher<T> {
  static enum Type {
    AND {
      @Override
      public <T> Predicate<T> apply(Collection<Predicate<T>> list) {
        
        return list.stream().reduce(Predicate::and).get();
      }
    },
    OR {
      @Override
      public <T> Predicate<T> apply(Collection<Predicate<T>> list) {
        return list.stream().reduce(Predicate::or).get();
      }
    };
    public abstract <T> Predicate<T> apply(Collection<Predicate<T>> list);
  }
  
  private final Type type;
  private final Matcher<T> parent;
  private final List<Predicate<T>> list;

  public BaseMatcher() {
    this(Type.AND);
  }
  
  public BaseMatcher(Type type) {
    this(type,null);
  }

  public BaseMatcher(Type type, Matcher<T> parent) {
    this(type,parent,Lists.newArrayList());
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
