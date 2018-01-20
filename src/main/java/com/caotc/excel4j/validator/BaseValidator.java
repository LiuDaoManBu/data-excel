package com.caotc.excel4j.validator;

import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.parse.error.ValidationError;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class BaseValidator<T> implements Validator<T> {
  private final ImmutableMap<? extends Predicate<T>, ? extends Function<T, String>> predicateToMessageFunctions;

  public BaseValidator(Predicate<T> predicate, Function<T, String> messageFunction) {
    this.predicateToMessageFunctions = ImmutableMap.of(predicate, messageFunction);
  }

  public BaseValidator(
      ImmutableMap<Predicate<T>, Function<T, String>> predicateToMessageFunctions) {
    super();
    this.predicateToMessageFunctions = predicateToMessageFunctions;
  }

  @Override
  public ImmutableCollection<ValidationError<T>> validate(T object) {
    return predicateToMessageFunctions.entrySet().stream()
        .filter(entry -> !entry.getKey().test(object))
        .map(entry -> new ValidationError<>(object, entry.getValue().apply(object)))
        .collect(ImmutableSet.toImmutableSet());
  }

}
