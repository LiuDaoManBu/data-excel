package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.constant.ConstructType;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.data.DataTypeMatcher;
import com.caotc.excel4j.matcher.data.constant.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.reflect.TypeToken;

public class DataConfig<V> {
  public static class Builder<V> {
    private MenuConfig<V> menuConfig;
    private Field field;
    private BaseDataType baseDataType;
    private DataType dataType;
    //TODO interface?
    private DataTypeMatcher.Builder matcherBuilder;
    private TypeToken<V> fieldType;
    private String fieldName;
    private ConstructType constructType;
    private LoadType loadType;
    private Integer dataNumber;
    //TODO 自由升降维?
    private Boolean beList;
    
    public DataConfig<V> build(){
      dataType=Optional.ofNullable(dataType).orElse(baseDataType);
      beList=Optional.ofNullable(beList).orElse(Boolean.FALSE);
      matcherBuilder.setDataType(dataType);
      //TODO 提示语
      Preconditions.checkState(Objects.nonNull(menuConfig));
      Preconditions.checkState(Objects.nonNull(dataType));
      return new DataConfig<V>(this);
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

    public DataType getDataType() {
      return dataType;
    }

    public Builder<V> setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    public TypeToken<V> getFieldType() {
      return fieldType;
    }

    public Builder<V> setFieldType(TypeToken<V> fieldType) {
      this.fieldType = fieldType;
      return this;
    }

    public String getFieldName() {
      return fieldName;
    }

    public Builder<V> setFieldName(String fieldName) {
      this.fieldName = fieldName;
      return this;
    }

    public ConstructType getConstructType() {
      return constructType;
    }

    public Builder<V> setConstructType(ConstructType constructType) {
      this.constructType = constructType;
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

    public Boolean getBeList() {
      return beList;
    }

    public Builder<V> setBeList(Boolean beList) {
      this.beList = beList;
      return this;
    }
    
  }
  
  public static <V> Builder<V> builder(){
    return new Builder<>();
  }
  
  private final MenuConfig<V> menuConfig;
  private final Field field;
  private final DataType dataType;
  private final DataMatcher dataMatcher;
  private final TypeToken<V> fieldType;
  private final String fieldName;
  private final ConstructType constructType;
  private final LoadType loadType;
  private final Integer dataNumber;
  //TODO 自由升降维?
  private final boolean beList;

  private DataConfig(Builder<V> builder) {
    this.menuConfig=builder.menuConfig;
    this.field=builder.field;
    this.dataType=builder.dataType;
    this.dataMatcher=builder.matcherBuilder.build();
    this.fieldType=builder.fieldType;
    this.fieldName=builder.fieldName;
    this.constructType=builder.constructType;
    this.loadType=builder.loadType;
    this.dataNumber=builder.dataNumber;
    this.beList=builder.beList;
  }
  
  @SuppressWarnings("unchecked")
  public V cast(Object value) {
    return (V) dataType.cast(value, fieldType.getRawType());
  }
  
  public boolean support(Object value) {
    return dataMatcher.support(value);
  }

  public boolean matches(Object value) {
    return dataMatcher.test(value);
  }

  public <T> boolean canCast(Class<T> clazz) {
    return dataType.canCast(clazz);
  }

  public <T> T cast(Object value, Class<T> clazz) {
    return dataType.cast(value, clazz);
  }

  public ImmutableCollection<TypeToken<?>> canCastTypes() {
    return dataType.canCastTypes();
  }

  public <T> boolean canCast(TypeToken<T> type) {
    return dataType.canCast(type);
  }

  public <T> T cast(Object value, TypeToken<T> type) {
    return dataType.cast(value, type);
  }

  public <T> ImmutableList<StandardCell> getDataCells(Menu<T> menu) {
    return loadType.getDataCells(menu);
  }

  public MenuConfig<V> getMenuConfig() {
    return menuConfig;
  }

  public DataMatcher getDataMatcher() {
    return dataMatcher;
  }

  public LoadType getLoadType() {
    return loadType;
  }

  public Integer getDataNumber() {
    return dataNumber;
  }

  public Field getField() {
    return field;
  }

  public TypeToken<V> getFieldType() {
    return fieldType;
  }

  public String getFieldName() {
    return fieldName;
  }

  public ConstructType getConstructType() {
    return constructType;
  }

  public boolean isBeList() {
    return beList;
  }
  
}
