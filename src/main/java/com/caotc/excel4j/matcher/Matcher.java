package com.caotc.excel4j.matcher;

import java.util.function.Function;
import java.util.function.Predicate;

public interface Matcher<T> extends Predicate<T> {
  Matcher<T> add(Predicate<T> predicate);

  <R> Matcher<T> add(Predicate<R> predicate, Function<T, R> transform);

  Matcher<T> and();

  Matcher<T> or();

  Matcher<T> endAnd();

  Matcher<T> endOr();
}
