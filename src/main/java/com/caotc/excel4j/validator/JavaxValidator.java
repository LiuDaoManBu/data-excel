package com.caotc.excel4j.validator;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.ValidatorFactory;
import com.caotc.excel4j.parse.error.ValidationError;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class JavaxValidator<T> implements Validator<T> {
  // TODO
  private static final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
  private final javax.validation.Validator validator;

  public JavaxValidator() {
    this.validator = factory.getValidator();
  }

  @Override
  public ImmutableCollection<ValidationError<T>> validate(T object) {
    return validator.validate(object).stream().map(ConstraintViolation::getMessage)
        .map(message -> new ValidationError<>(object, message))
        .collect(ImmutableSet.toImmutableSet());
  }

}
