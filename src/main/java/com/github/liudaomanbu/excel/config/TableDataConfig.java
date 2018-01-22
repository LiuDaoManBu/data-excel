package com.github.liudaomanbu.excel.config;

import java.util.List;
import java.util.Map;
import com.github.liudaomanbu.excel.constant.ConstructType;
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
    private ConstructType constructType;
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

    public ConstructType getConstructType() {
      return constructType;
    }

    public Builder<T> setConstructType(ConstructType constructType) {
      this.constructType = constructType;
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
  private final ConstructType constructType;
  private final ImmutableList<Validator<Map<Menu<T>, StandardCell>>> validators;

  protected TableDataConfig(Builder<T> builder) {
    constructType = builder.constructType;
    tableConfig = builder.tableConfig;
    Preconditions.checkNotNull(tableConfig, "tableConfig can't be null");
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
  }

  public ConstructType getConstructType() {
    return constructType;
  }

  public TableConfig<T> getTableConfig() {
    return tableConfig;
  }

  public ImmutableList<Validator<Map<Menu<T>, StandardCell>>> getValidators() {
    return validators;
  }

}
