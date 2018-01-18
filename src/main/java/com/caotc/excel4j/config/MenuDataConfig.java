package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.constant.Type;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.reflect.TypeToken;

public class MenuDataConfig{
  public static class Builder{
    private MenuConfig menuConfig;
    private Field field;
    private BaseDataType baseDataType;
    private DataType dataType;
    // TODO interface?
    private StandardCellMatcher.Builder matcherBuilder;
    private String fieldName;
    private LoadType loadType;
    private Integer dataNumber;

    public MenuDataConfig build() {
      // TODO
//      setType(Optional.ofNullable(field).map(Field::getType).map(type -> (Class) type)
//          .map(TypeToken::of).orElse(null));
      dataType = Optional.ofNullable(dataType).orElse(baseDataType);
      loadType = Optional.ofNullable(loadType).orElse(DEFAULT_LOAD_TYPE);

      StandardCellMatcher.Builder builder = StandardCellMatcher.builder();
      builder.setDataType(dataType).setType(Type.AND).setMessageFunction(cell -> JOINER.join(cell.formatAsString(),"数据格式不正确"))
          .add(dataType::test);
      Optional.ofNullable(matcherBuilder).ifPresent(t -> builder.add(t.setDataType(dataType)));
      matcherBuilder = builder;
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

    public StandardCellMatcher.Builder getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder setMatcherBuilder(StandardCellMatcher.Builder matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
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

    public Integer getDataNumber() {
      return dataNumber;
    }

    public Builder setDataNumber(Integer dataNumber) {
      this.dataNumber = dataNumber;
      return this;
    }

  }
  private static final Joiner JOINER=Joiner.on("");
  private static final LoadType DEFAULT_LOAD_TYPE = LoadType.UNFIXED;

  public static  Builder builder() {
    return new Builder();
  }

  private final MenuConfig menuConfig;
  private final LoadType loadType;
  private final Field field;
  private final String fieldName;
  private final Integer dataNumber;
  private final DataType dataType;
  private final StandardCellMatcher matcher;

  protected MenuDataConfig(Builder builder) {
    this.menuConfig = builder.menuConfig;
    this.loadType = builder.loadType;
    this.field = builder.field;
    this.fieldName = builder.fieldName;
    this.dataNumber = builder.dataNumber;
    this.dataType = builder.dataType;
    this.matcher = builder.matcherBuilder.build();
  }

  public <T> T cast(Object value,TypeToken<T> type) {
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

  public Integer getDataNumber() {
    return dataNumber;
  }

  public StandardCellMatcher getMatcher() {
    return matcher;
  }

  public DataType getDataType() {
    return dataType;
  }
}
