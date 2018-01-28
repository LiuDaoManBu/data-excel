package com.github.liudaomanbu.excel.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;
import com.github.liudaomanbu.excel.constant.LoadType;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.matcher.data.type.DataType;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.StandardCell;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.reflect.TypeToken;

public class MenuDataConfig<T> {
  public static class Builder<T> {
    private MenuConfig<T> menuConfig;
    private Field field;
    private BaseDataType baseDataType;
    private DataType dataType;
    private List<Validator<StandardCell>> validators;
    private String fieldName;
    private LoadType loadType;

    public Builder() {
      validators = Lists.newLinkedList();
    }

    public MenuDataConfig<T> build() {
      dataType = Optional.ofNullable(dataType).orElse(baseDataType);
      return new MenuDataConfig<>(this);
    }

    public MenuConfig<T> getMenuConfig() {
      return menuConfig;
    }

    public Builder<T> setMenuConfig(MenuConfig<T> menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Field getField() {
      return field;
    }

    public Builder<T> setField(Field field) {
      this.field = field;
      return this;
    }

    public BaseDataType getBaseDataType() {
      return baseDataType;
    }

    public Builder<T> setBaseDataType(BaseDataType baseDataType) {
      this.baseDataType = baseDataType;
      return this;
    }

    public DataType getDataType() {
      return dataType;
    }

    public Builder<T> setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    public String getFieldName() {
      return fieldName;
    }

    public Builder<T> setFieldName(String fieldName) {
      this.fieldName = fieldName;
      return this;
    }

    public LoadType getLoadType() {
      return loadType;
    }

    public Builder<T> setLoadType(LoadType loadType) {
      this.loadType = loadType;
      return this;
    }

    public List<Validator<StandardCell>> getValidators() {
      return validators;
    }

    public Builder<T> setValidators(List<Validator<StandardCell>> validators) {
      this.validators = validators;
      return this;
    }

  }

  private static final LoadType DEFAULT_LOAD_TYPE = LoadType.UNFIXED;

  public static <T> Builder<T> builder() {
    return new Builder<>();
  }

  private final MenuConfig<T> menuConfig;
  private final LoadType loadType;
  private final Field field;
  private final String fieldName;
  private final DataType dataType;
  private final ImmutableList<Validator<StandardCell>> validators;

  protected MenuDataConfig(Builder<T> builder) {
    menuConfig = builder.menuConfig;
    Preconditions.checkNotNull(menuConfig, "menuConfig can't be null");
    loadType = Optional.ofNullable(builder.loadType).orElse(DEFAULT_LOAD_TYPE);
    field = builder.field;
    fieldName = Optional.ofNullable(builder.fieldName).orElseGet(field::getName);
    dataType = builder.dataType;
    Preconditions.checkNotNull(dataType, "dataType can't be null");

    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());

  }

  public <V> V cast(Object value, TypeToken<V> type) {
    return dataType.cast(value, type);
  }

  public ImmutableList<StandardCell> getDataCells(Menu<T> menu) {
    return loadType.getDataCells(menu);
  }

  public LoadType getLoadType() {
    return loadType;
  }

  public Field getField() {
    return field;
  }

  public String getFieldName() {
    return fieldName;
  }

  public MenuConfig<T> getMenuConfig() {
    return menuConfig;
  }

  public DataType getDataType() {
    return dataType;
  }

  public ImmutableList<Validator<StandardCell>> getValidators() {
    return validators;
  }
}
