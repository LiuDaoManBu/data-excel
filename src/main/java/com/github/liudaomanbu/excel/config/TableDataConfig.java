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
      validators.add(new JavaxValidator<>(validator, type, groups));
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
    Preconditions.checkNotNull(tableConfig, "tableConfig can't be null");
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
