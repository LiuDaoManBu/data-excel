package com.github.liudaomanbu.excel.config;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import com.github.liudaomanbu.excel.parse.result.Data;
import com.github.liudaomanbu.excel.validator.JavaxValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

public class TableDataConfig<T> {
  public static class Builder<T> {
    private TableConfig<T> tableConfig;
    private Class<T> type;
    private List<Validator<Data<T>>> validators;
    private Consumer<Map<String, Object>> beforeTransform;
    private Consumer<T> beforeValidator;

    public Builder() {
      validators = Lists.newLinkedList();
    }

    public TableDataConfig<T> build() {
      return new TableDataConfig<>(this);
    }

    public Builder<T> addValidator(javax.validation.Validator validator, Class<?>... groups) {
      validators.add(new JavaxValidator<T>(validator, groups));
      return this;
    }

    public TableConfig<T> getTableConfig() {
      return tableConfig;
    }

    public Builder<T> setTableConfig(TableConfig<T> tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }


    public Class<T> getType() {
      return type;
    }

    public Builder<T> setType(Class<T> type) {
      this.type = type;
      return this;
    }

    public List<Validator<Data<T>>> getValidators() {
      return validators;
    }

    public Builder<T> setValidators(List<Validator<Data<T>>> validators) {
      this.validators = validators;
      return this;
    }

    public Consumer<T> getBeforeValidator() {
      return beforeValidator;
    }

    public Builder<T> setBeforeValidator(Consumer<T> beforeValidator) {
      this.beforeValidator = beforeValidator;
      return this;
    }

    public Consumer<Map<String, Object>> getBeforeTransform() {
      return beforeTransform;
    }

    public Builder<T> setBeforeTransform(Consumer<Map<String, Object>> beforeTransform) {
      this.beforeTransform = beforeTransform;
      return this;
    }

  }

  public static <T> Builder<T> builder() {
    return new Builder<>();
  }

  private final TableConfig<T> tableConfig;
  private final Class<T> type;
  private final ImmutableList<Validator<Data<T>>> validators;
  private final Consumer<Map<String, Object>> beforeTransform;
  private final Consumer<T> beforeValidator;

  protected TableDataConfig(Builder<T> builder) {
    type = builder.type;
    tableConfig = builder.tableConfig;
    Preconditions.checkNotNull(tableConfig, "tableConfig can't be null");
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
    beforeTransform = builder.beforeTransform;
    beforeValidator = builder.beforeValidator;
  }

  public Class<T> getType() {
    return type;
  }

  public TableConfig<T> getTableConfig() {
    return tableConfig;
  }

  public ImmutableList<Validator<Data<T>>> getValidators() {
    return validators;
  }

  public Consumer<Map<String, Object>> getBeforeTransform() {
    return beforeTransform;
  }

  public Consumer<T> getBeforeValidator() {
    return beforeValidator;
  }

}
