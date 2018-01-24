package com.github.liudaomanbu.excel.validator;

import java.util.Map;
import javax.validation.ConstraintViolation;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.StandardCell;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class JavaxValidator<T> implements Validator<Map<Menu<T>, StandardCell>> {

  private final javax.validation.Validator validator;
  private final Class<T> type;
  private final Class<?>[] groups;

  public JavaxValidator(javax.validation.Validator validator, Class<T> type, Class<?>... groups) {
    super();
    this.validator = validator;
    this.type = type;
    this.groups = groups;
  }

  @Override
  public ImmutableCollection<ValidationError<Map<Menu<T>, StandardCell>>> validate(
      Map<Menu<T>, StandardCell> object) {
    T value = ExcelUtil.toJavaObject(object, type);
    ImmutableMap<Menu<T>, String> menuToMessages =
        validator
            .validate(value,
                groups)
            .stream()
            .collect(ImmutableMap.toImmutableMap(t -> object.keySet().stream()
                .filter(menu -> t.getPropertyPath().toString().equals(menu.getFieldName()))
                .findAny().get(), ConstraintViolation::getMessage));
    return menuToMessages.entrySet().stream()
        .map(entry -> entry.getKey().getFullName() + object.get(entry.getKey()).formatAsString()
            + "单元格" + entry.getValue())
        .map(message -> new ValidationError<>(object, message))
        .collect(ImmutableSet.toImmutableSet());
  }

}
