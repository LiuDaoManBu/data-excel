package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import com.google.common.collect.Lists;

public class BaseMatcher<T> implements Matcher<T> {
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
  public Matcher<T> add(Type type) {
    Matcher<T> predicateList = new BaseMatcher<T>(type, this, Lists.newArrayList());
    list.add(predicateList);
    return predicateList;
  }

  @Override
  public Matcher<T> not() {
    return add(Type.NOT);
  }

  @Override
  public Matcher<T> and() {
    return add(Type.AND);
  }

  @Override
  public Matcher<T> or() {
    return add(Type.OR);
  }

  public Matcher<T> end() {
    return Optional.ofNullable(parent).orElse(this);                                        
  }
}
