package com.github.liudaomanbu.excel.validator;

import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.TableData;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class JavaxValidator<T> implements Validator<TableData<T>.Data> {

  private final javax.validation.Validator validator;
  private final Class<?>[] groups;

  public JavaxValidator(javax.validation.Validator validator, Class<?>... groups) {
    super();
    this.validator = validator;
    this.groups = groups;
  }

  @Override
  public boolean premise(TableData<T>.Data value) {
    return true;
  }

  @Override
  public ImmutableCollection<ValidationError<TableData<T>.Data>> validate(
      TableData<T>.Data object) {
    T value = object.getValue();
    return validator.validate(value, groups).stream().map(violation -> {
      Menu<T> menu = object.getMenuByFieldName(violation.getPropertyPath().toString());
      return menu.getFullName() + object.getMenuToValueCells().get(menu).formatAsString() + "单元格"
          + violation.getMessage();
    }).map(message -> new ValidationError<>(object, message))
        .collect(ImmutableSet.toImmutableSet());
  }

}
