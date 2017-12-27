package com.caotc.excel4j.matcher;

import java.util.Collection;
import java.util.function.Predicate;

public enum Type {
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
