package com.github.liudaomanbu.excel.config;

import java.util.List;
import com.github.liudaomanbu.excel.parse.result.TableData;
import com.github.liudaomanbu.excel.validator.JavaxValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

public class TableDataConfig<T> {

  public static class Builder<T> {
    private TableConfig<T> tableConfig;
    private Class<T> type;
    private List<Validator<TableData<T>.Data>> validators;

    public Builder() {
      validators = Lists.newLinkedList();
    }

    public TableDataConfig<T> build() {
      return new TableDataConfig<>(this);
    }

    public Builder<T> addJavaxValidator(javax.validation.Validator validator,Class<?>... groups) {
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

    public List<Validator<TableData<T>.Data>> getValidators() {
      return validators;
    }

    public Builder<T> setValidators(List<Validator<TableData<T>.Data>> validators) {
      this.validators = validators;
      return this;
    }

  }
  
  public static <T> Builder<T> builder(){
    return new Builder<>();
  }
  
  private final TableConfig<T> tableConfig;
  private final Class<T> type;
  private final ImmutableList<Validator<TableData<T>.Data>> validators;

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

  public ImmutableList<Validator<TableData<T>.Data>> getValidators() {
    return validators;
  }

}
