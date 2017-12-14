package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import com.caotc.excel4j.constant.ConstructType;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.data.NativeDataMatcher;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.reflect.TypeToken;

public class DataConfig<V> {
  private MenuConfig<V> menuConfig;
  private Field field;
  private NativeDataMatcher dataMatcher;
  private TypeToken<V> fieldType;
  private String fieldName;
  private ConstructType constructType;
  
  private LoadType loadType;
  private Integer dataNumber;
  //TODO 自由升降维?
  private boolean beList;

  @SuppressWarnings("unchecked")
  public V cast(Object value) {
    return (V) dataMatcher.cast(value, fieldType.getRawType());
  }
  
  public boolean support(Object value) {
    return dataMatcher.support(value);
  }

  public boolean matches(Object value) {
    return dataMatcher.test(value);
  }

  public <T> boolean canCast(Class<T> clazz) {
    return dataMatcher.canCast(clazz);
  }

  public <T> T cast(Object value, Class<T> clazz) {
    return dataMatcher.cast(value, clazz);
  }

  public ImmutableCollection<TypeToken<?>> canCastTypes() {
    return dataMatcher.canCastTypes();
  }

  public <T> boolean canCast(TypeToken<T> type) {
    return dataMatcher.canCast(type);
  }

  public <T> T cast(Object value, TypeToken<T> type) {
    return dataMatcher.cast(value, type);
  }

  public <T> ImmutableList<StandardCell> getDataCells(Menu<T> menu) {
    return loadType.getDataCells(menu);
  }

  public MenuConfig<V> getMenuConfig() {
    return menuConfig;
  }

  public NativeDataMatcher getDataMatcher() {
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
