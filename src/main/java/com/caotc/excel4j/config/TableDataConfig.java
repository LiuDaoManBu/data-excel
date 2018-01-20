package com.caotc.excel4j.config;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import javax.validation.ConstraintViolation;
import com.caotc.excel4j.constant.ConstructType;
import com.caotc.excel4j.parse.error.ValidationError;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.caotc.excel4j.validator.Validator;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class TableDataConfig {

  public static class Builder {
    private TableConfig tableConfig;
    private ConstructType constructType;
    private List<Validator<Map<Menu, StandardCell>>> validators;

    public Builder() {
      validators = Lists.newLinkedList();
    }

    public TableDataConfig build() {
      return new TableDataConfig(this);
    }

    public <T> Builder addJavaxValidator(javax.validation.Validator validator, Class<T> type,
        Class<?>... groups) {
      validators.add(new Validator<Map<Menu, StandardCell>>() {
        @Override
        public ImmutableCollection<ValidationError<Map<Menu, StandardCell>>> validate(
            Map<Menu, StandardCell> object) {
          T v = ExcelUtil.toJavaObject(object, type);
          ImmutableMap<Menu, String> menuToMessages =
              validator.validate(v, groups).stream()
                  .collect(
                      ImmutableMap.toImmutableMap(
                          t -> object.keySet().stream()
                              .filter(menu -> t.getPropertyPath().toString()
                                  .equals(menu.getFieldName()))
                              .findAny().get(),
                          ConstraintViolation::getMessage));
          return menuToMessages.entrySet().stream()
              .map(entry -> entry.getKey().getName() + object.get(entry.getKey()).formatAsString()
                  + entry.getValue())
              .map(message -> new ValidationError<>(object, message))
              .collect(ImmutableSet.toImmutableSet());
        }
      });
      return this;
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public Builder setTableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public ConstructType getConstructType() {
      return constructType;
    }

    public Builder setConstructType(ConstructType constructType) {
      this.constructType = constructType;
      return this;
    }

    public List<Validator<Map<Menu, StandardCell>>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<Map<Menu, StandardCell>>> validators) {
      this.validators = validators;
      return this;
    }

  }

  private final TableConfig tableConfig;
  private final ConstructType constructType;
  private final ImmutableList<Validator<Map<Menu, StandardCell>>> validators;

  protected TableDataConfig(Builder builder) {
    constructType = builder.constructType;
    tableConfig = builder.tableConfig;
    // TODO tip
    Preconditions.checkState(Objects.nonNull(tableConfig));
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
  }

  public ConstructType getConstructType() {
    return constructType;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ImmutableList<Validator<Map<Menu, StandardCell>>> getValidators() {
    return validators;
  }

}
