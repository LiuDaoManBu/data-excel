package com.caotc.excel4j.matcher.constant;

import java.util.function.Predicate;
import com.google.common.collect.Streams;

public enum Type {
  AND {
    @Override
    public <T> Predicate<T> reduce(Iterable<Predicate<T>> predicates) {
      return Streams.stream(predicates).reduce(Predicate::and).get();
    }
  },
  OR {
    @Override
    public <T> Predicate<T> reduce(Iterable<Predicate<T>> predicates) {
      return Streams.stream(predicates).reduce(Predicate::or).get();
    }
  };
  public abstract <T> Predicate<T> reduce(Iterable<Predicate<T>> predicates);
}
