package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;

public interface Matcher<T> extends Predicate<T> {
  public static interface Builder<T>{
    Matcher<T> build();
  }
  Matcher<T> add(Predicate<T> predicate);

  <R> Matcher<T> add(Predicate<R> predicate, Function<T, R> transform);

  Matcher<T> add(StringMatcherType type, String predicateValue, Function<T, String> transform);

  <R extends Comparable<R>> Matcher<T> add(ComparableMatcherType type, R predicateValue,
      Function<T, R> transform);

  Matcher<T> and();

  Matcher<T> or();

  Matcher<T> endAnd();

  Matcher<T> endOr();
  
  Optional<String> match(T value);
  
  Function<T, String> getMessageFunction();
  
  String getMessage(T value);
  
  List<Predicate<T>> getPredicates();
}
