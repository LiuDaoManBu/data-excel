package com.github.liudaomanbu.excel.validator;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class BaseValidator<T> implements Validator<T> {
  private static <T> Predicate<T> getDefaultPremisePredicate() {
    return Objects::nonNull;
  }

  private final Predicate<T> premisePredicate;
  private final ImmutableMap<? extends Predicate<T>, ? extends Function<T, String>> predicateToMessageFunctions;

  public BaseValidator(Predicate<T> predicate, Function<T, String> messageFunction) {
    this(predicate, messageFunction, getDefaultPremisePredicate());
  }

  public BaseValidator(
      ImmutableMap<Predicate<T>, Function<T, String>> predicateToMessageFunctions) {
    this(predicateToMessageFunctions, getDefaultPremisePredicate());
  }

  public BaseValidator(Predicate<T> predicate, Function<T, String> messageFunction,
      Predicate<T> premisePredicate) {
    this.predicateToMessageFunctions = ImmutableMap.of(predicate, messageFunction);
    this.premisePredicate = premisePredicate;
  }

  public BaseValidator(ImmutableMap<Predicate<T>, Function<T, String>> predicateToMessageFunctions,
      Predicate<T> premisePredicate) {
    this.predicateToMessageFunctions = predicateToMessageFunctions;
    this.premisePredicate = premisePredicate;
  }

  @Override
  public boolean premise(T value) {
    return premisePredicate.test(value);
  }

  @Override
  public ImmutableCollection<ValidationError<T>> validate(T value) {
    return predicateToMessageFunctions.entrySet().stream()
        .filter(entry -> !entry.getKey().test(value))
        .map(entry -> new ValidationError<>(value, entry.getValue().apply(value)))
        .collect(ImmutableSet.toImmutableSet());
  }
}
