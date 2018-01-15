package com.caotc.excel4j.matcher.constant;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
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
      Optional<Predicate<T>> optional =
          matcher.getPredicates().stream().filter(predicate -> !predicate.test(value)).findFirst();
      if (optional.isPresent()) {
        Predicate<T> predicate = optional.get();
        // TODO 消灭instanceof?
        if (predicate instanceof Matcher) {
          Optional<String> result=((Matcher<T>) predicate).match(value);
          
          return result;
        } else {
          Optional<String> result=Optional.ofNullable(matcher.getEffectiveMessageFunction())
              .map(f -> {
                return f.apply(value);
              });
          return result;
        }
      }
      return Optional.empty();
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
          ? Optional.ofNullable(matcher.getEffectiveMessageFunction()).map(f -> f.apply(value))
          : Optional.empty();
    }
  };
  public abstract <T> Predicate<T> apply(Iterable<Predicate<T>> list);

  public abstract <T> Optional<String> apply(Matcher<T> matcher, T value);
}
