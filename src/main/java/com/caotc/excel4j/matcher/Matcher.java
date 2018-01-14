package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;

public interface Matcher<T> extends Predicate<T> {
  public static interface Builder<T> {
    Builder<T> add(Predicate<T> predicate);

    Builder<T> add(Builder<T> builder);
    
    <R> Builder<T> add(Predicate<R> predicate, Function<T, R> transform);

    Builder<T> add(StringMatcherType type, String predicateValue, Function<T, String> transform);

    <R extends Comparable<R>> Builder<T> add(ComparableMatcherType type, R predicateValue,
        Function<T, R> transform);

    Builder<T> and();

    Builder<T> or();

    Builder<T> endAnd();

    Builder<T> endOr();

    Matcher<T> build();
  }

  Optional<String> match(T value);

  Function<T, String> getMessageFunction();

  List<Predicate<T>> getPredicates();
}
