package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.constant.Type;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.reflect.TypeToken;

public class MenuDataConfig<V> extends DataConfig<V> {
  public static class Builder<V> extends DataConfig.Builder<V> {
    private MenuConfig<V> menuConfig;
    private Field field;
    private BaseDataType baseDataType;
    private DataType dataType;
    // TODO interface?
    private StandardCellMatcher.Builder matcherBuilder;
    private String fieldName;
    private LoadType loadType;
    private Integer dataNumber;

    public MenuDataConfig<V> build() {
      // TODO
      setType(Optional.ofNullable(field).map(Field::getType).map(type -> (Class<V>) type)
          .map(TypeToken::of).orElse(null));
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

      return new MenuDataConfig<>(this);
    }

    public MenuConfig<V> getMenuConfig() {
      return menuConfig;
    }

    public Builder<V> setMenuConfig(MenuConfig<V> menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Field getField() {
      return field;
    }

    public Builder<V> setField(Field field) {
      this.field = field;
      return this;
    }

    public BaseDataType getBaseDataType() {
      return baseDataType;
    }

    public Builder<V> setBaseDataType(BaseDataType baseDataType) {
      this.baseDataType = baseDataType;
      return this;
    }

    public DataType getDataType() {
      return dataType;
    }

    public Builder<V> setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    public StandardCellMatcher.Builder getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder<V> setMatcherBuilder(StandardCellMatcher.Builder matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
      return this;
    }

    public String getFieldName() {
      return fieldName;
    }

    public Builder<V> setFieldName(String fieldName) {
      this.fieldName = fieldName;
      return this;
    }

    public LoadType getLoadType() {
      return loadType;
    }

    public Builder<V> setLoadType(LoadType loadType) {
      this.loadType = loadType;
      return this;
    }

    public Integer getDataNumber() {
      return dataNumber;
    }

    public Builder<V> setDataNumber(Integer dataNumber) {
      this.dataNumber = dataNumber;
      return this;
    }

  }
  private static final Joiner JOINER=Joiner.on("");
  private static final LoadType DEFAULT_LOAD_TYPE = LoadType.UNFIXED;

  public static <V> Builder<V> builder() {
    return new Builder<>();
  }

  private final MenuConfig<V> menuConfig;
  private final LoadType loadType;
  private final Field field;
  private final String fieldName;
  private final Integer dataNumber;
  private final DataType dataType;
  private final StandardCellMatcher matcher;

  protected MenuDataConfig(Builder<V> builder) {
    super(builder);
    this.menuConfig = builder.menuConfig;
    this.loadType = builder.loadType;
    this.field = builder.field;
    this.fieldName = builder.fieldName;
    this.dataNumber = builder.dataNumber;
    this.dataType = builder.dataType;
    this.matcher = builder.matcherBuilder.build();
  }

  public V cast(Object value) {
    return dataType.cast(value, getType());
  }
//
//  public ImmutableList<StandardCell> getDataCells(Menu<?,V> menu) {
//    return loadType.getDataCells(menu);
//  }

  public LoadType getLoadType() {
    return loadType;
  }

  public Field getField() {
    return field;
  }

  public String getFieldName() {
    return fieldName;
  }

  public MenuConfig<V> getMenuConfig() {
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
