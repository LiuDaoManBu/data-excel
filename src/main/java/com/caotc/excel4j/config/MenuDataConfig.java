package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.BaseValidator;
import com.caotc.excel4j.matcher.Validator;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.reflect.TypeToken;

public class MenuDataConfig {
  public static class Builder {
    private MenuConfig menuConfig;
    private Field field;
    private BaseDataType baseDataType;
    private DataType dataType;
    private List<Validator<StandardCell>> validators;
    private String fieldName;
    private LoadType loadType;

    public MenuDataConfig build() {
      // TODO
      // setType(Optional.ofNullable(field).map(Field::getType).map(type -> (Class) type)
      // .map(TypeToken::of).orElse(null));

      dataType = Optional.ofNullable(dataType).orElse(baseDataType);
      loadType = Optional.ofNullable(loadType).orElse(DEFAULT_LOAD_TYPE);
      
      validators.add(new BaseValidator<StandardCell>(
          ImmutableMap.<Predicate<StandardCell>, Function<StandardCell, String>>builder()
              .put(cell -> dataType.test(cell.getValue()),
                  cell -> JOINER.join(cell.formatAsString(), "数据格式不正确"))
              .build()));
      // TODO 提示语
      Preconditions.checkState(Objects.nonNull(menuConfig));
      Preconditions.checkState(Objects.nonNull(dataType));

      return new MenuDataConfig(this);
    }

    public MenuConfig getMenuConfig() {
      return menuConfig;
    }

    public Builder setMenuConfig(MenuConfig menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Field getField() {
      return field;
    }

    public Builder setField(Field field) {
      this.field = field;
      return this;
    }

    public BaseDataType getBaseDataType() {
      return baseDataType;
    }

    public Builder setBaseDataType(BaseDataType baseDataType) {
      this.baseDataType = baseDataType;
      return this;
    }

    public DataType getDataType() {
      return dataType;
    }

    public Builder setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    public String getFieldName() {
      return fieldName;
    }

    public Builder setFieldName(String fieldName) {
      this.fieldName = fieldName;
      return this;
    }

    public LoadType getLoadType() {
      return loadType;
    }

    public Builder setLoadType(LoadType loadType) {
      this.loadType = loadType;
      return this;
    }

    public List<Validator<StandardCell>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<StandardCell>> validators) {
      this.validators = validators;
      return this;
    }

  }

  private static final Joiner JOINER = Joiner.on("");
  private static final LoadType DEFAULT_LOAD_TYPE = LoadType.UNFIXED;

  public static Builder builder() {
    return new Builder();
  }

  private final MenuConfig menuConfig;
  private final LoadType loadType;
  private final Field field;
  private final String fieldName;
  private final DataType dataType;
  private final ImmutableList<Validator<StandardCell>> validators;

  protected MenuDataConfig(Builder builder) {
    menuConfig = builder.menuConfig;
    loadType = builder.loadType;
    field = builder.field;
    fieldName = builder.fieldName;
    dataType = builder.dataType;
    validators=builder.validators.stream().collect(ImmutableList.toImmutableList());
  }

  public <T> T cast(Object value, TypeToken<T> type) {
    return dataType.cast(value, type);
  }

  public ImmutableList<StandardCell> getDataCells(Menu menu) {
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

  public MenuConfig getMenuConfig() {
    return menuConfig;
  }

  public DataType getDataType() {
    return dataType;
  }

  public ImmutableList<Validator<StandardCell>> getValidators() {
    return validators;
  }
}
