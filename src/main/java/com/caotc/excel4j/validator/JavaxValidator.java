package com.caotc.excel4j.validator;

import java.util.Map;
import javax.validation.ConstraintViolation;
import com.caotc.excel4j.parse.error.ValidationError;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class JavaxValidator<T> implements Validator<Map<Menu, StandardCell>> {

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
  public ImmutableCollection<ValidationError<Map<Menu, StandardCell>>> validate(
      Map<Menu, StandardCell> object) {
    T value = ExcelUtil.toJavaObject(object, type);
    ImmutableMap<Menu, String> menuToMessages =
        validator
            .validate(value,
                groups)
            .stream()
            .collect(ImmutableMap.toImmutableMap(t -> object.keySet().stream()
                .filter(menu -> t.getPropertyPath().toString().equals(menu.getFieldName()))
                .findAny().get(), ConstraintViolation::getMessage));
    return menuToMessages.entrySet().stream()
        .map(entry -> entry.getKey().getName() + object.get(entry.getKey()).formatAsString()
            + entry.getValue())
        .map(message -> new ValidationError<>(object, message))
        .collect(ImmutableSet.toImmutableSet());
  }

}
