package com.github.liudaomanbu.excel.config;

import java.util.List;
import java.util.Map;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.StandardCell;
import com.github.liudaomanbu.excel.validator.JavaxValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

public class TableDataConfig<T> {

  public static class Builder<T> {
    private TableConfig<T> tableConfig;
    private Class<T> type;
    private List<Validator<Map<Menu<T>, StandardCell>>> validators;

    public Builder() {
      validators = Lists.newLinkedList();
    }

    public TableDataConfig<T> build() {
      return new TableDataConfig<>(this);
    }

    public Builder<T> addJavaxValidator(javax.validation.Validator validator, Class<T> type,
        Class<?>... groups) {
      validators.add(new JavaxValidator<>(validator, type, groups));
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

    public List<Validator<Map<Menu<T>, StandardCell>>> getValidators() {
      return validators;
    }

    public Builder<T> setValidators(List<Validator<Map<Menu<T>, StandardCell>>> validators) {
      this.validators = validators;
      return this;
    }

  }

  private final TableConfig<T> tableConfig;
  private final Class<T> type;
  private final ImmutableList<Validator<Map<Menu<T>, StandardCell>>> validators;

  protected TableDataConfig(Builder<T> builder) {
    type = builder.type;
    tableConfig = builder.tableConfig;
    Preconditions.checkNotNull(tableConfig, "tableConfig can't be null");
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
  }

  public Class<T> getType() {
    return type;
  }

  public TableConfig<T> getTableConfig() {
    return tableConfig;
  }

  public ImmutableList<Validator<Map<Menu<T>, StandardCell>>> getValidators() {
    return validators;
  }

}
