package com.github.liudaomanbu.excel.validator;

import com.github.liudaomanbu.excel.parse.result.Data;
import com.google.common.collect.ImmutableMap;
import java.util.function.Function;
import java.util.function.Predicate;

public class Validators {
  public static <T> Validator<T> create(Predicate<T> predicate, Function<T, String> messageFunction){
    return new BaseValidator<>(predicate,messageFunction);
  }

  public static <T> Validator<T> create(Predicate<T> predicate, Function<T, String> messageFunction,Predicate<T> premisePredicate){
    return new BaseValidator<>(predicate,messageFunction,premisePredicate);
  }

  public static <T> Validator<T> create(ImmutableMap<Predicate<T>, Function<T, String>> predicateToMessageFunctions){
    return new BaseValidator<>(predicateToMessageFunctions);
  }

  public static <T> Validator<T> create(ImmutableMap<Predicate<T>, Function<T, String>> predicateToMessageFunctions,Predicate<T> premisePredicate){
    return new BaseValidator<>(predicateToMessageFunctions,premisePredicate);
  }

  public static <T> Validator<Data<T>> create(javax.validation.Validator validator, Class<?>... groups){
    return new JavaxValidatorAdapter<>(validator,groups);
  }

  private Validators() {
    throw new AssertionError();
  }
}
