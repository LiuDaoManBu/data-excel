package com.github.liudaomanbu.excel.validator;

import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.parse.result.Data;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class JavaxValidator<T> implements Validator<Data<T>> {

  private final javax.validation.Validator validator;
  private final Class<?>[] groups;

  public JavaxValidator(javax.validation.Validator validator, Class<?>... groups) {
    super();
    this.validator = validator;
    this.groups = groups;
  }

  @Override
  public boolean needValidate(Data<T> value) {
    return true;
  }

  @Override
  public ImmutableCollection<ValidationError<Data<T>>> validate(
      Data<T> object) {
    T value = object.getValue();
    return validator.validate(value, groups).stream().map(violation -> {
      Menu<T> menu = object.getMenuByFieldName(violation.getPropertyPath().toString());
      return menu.getFullName() + object.getMenuToValueCells().get(menu).formatAsString() + "单元格"
          + violation.getMessage();
    }).map(message -> new ValidationError<>(object, message))
        .collect(ImmutableSet.toImmutableSet());
  }

}
