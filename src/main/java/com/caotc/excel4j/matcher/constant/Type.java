package com.caotc.excel4j.matcher.constant;

import java.util.function.Predicate;
import com.google.common.collect.Streams;

public enum Type {
  AND {
    @Override
    public <T> Predicate<T> apply(Iterable<Predicate<T>> list) {
      return Streams.stream(list).reduce(Predicate::and).get();
    }
  },
  OR {
    @Override
    public <T> Predicate<T> apply(Iterable<Predicate<T>> list) {
      return Streams.stream(list).reduce(Predicate::or).get();
    }
  };
  public abstract <T> Predicate<T> apply(Iterable<Predicate<T>> list);
}
