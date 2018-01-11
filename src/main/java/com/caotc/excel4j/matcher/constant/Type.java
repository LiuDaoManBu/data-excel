package com.caotc.excel4j.matcher.constant;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.Matcher;
import com.google.common.collect.Streams;

public enum Type {
  AND {
    @Override
    public <T> Predicate<T> apply(Iterable<Predicate<T>> list) {
      return Streams.stream(list).reduce(Predicate::and).get();
    }

    @Override
    public <T> Optional<String> apply(Matcher<T> matcher, T value) {
      return matcher.getPredicates().stream().filter(predicate -> !predicate.test(value))
          .findFirst()
          .map(predicate -> predicate instanceof Matcher
              && Objects.nonNull(((Matcher<T>) predicate).getMessageFunction())
                  ? (Matcher<T>) predicate
                  : matcher)
          .map(m -> m.getMessage(value));
    }
  },
  OR {
    @Override
    public <T> Predicate<T> apply(Iterable<Predicate<T>> list) {
      return Streams.stream(list).reduce(Predicate::or).get();
    }

    @Override
    public <T> Optional<String> apply(Matcher<T> matcher, T value) {
      return matcher.getPredicates().stream().noneMatch(p -> p.test(value))
          ? Optional.ofNullable(matcher.getMessage(value))
          : Optional.empty();
    }
  };
  public abstract <T> Predicate<T> apply(Iterable<Predicate<T>> list);

  public abstract <T> Optional<String> apply(Matcher<T> matcher, T value);
}
