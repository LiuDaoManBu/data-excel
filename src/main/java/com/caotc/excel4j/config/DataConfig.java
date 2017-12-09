package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.Collection;
import com.caotc.excel4j.constant.ConstructType;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.collect.ImmutableList;
import com.google.common.reflect.TypeToken;

public class DataConfig<V> {
  private MenuConfig<V> menuConfig;
  private Field field;
  private DataMatcher dataMatcher;
  private TypeToken<V> fieldType;
  private String fieldName;
  private ConstructType castType;
  
  private LoadType loadType;
  private Integer dataNumber;
  //TODO 自由升降维?
  private boolean beList;

  public V cast(Object value) {
    return (V) dataMatcher.cast(value, fieldType.getRawType());
  }
  
  public boolean support(Object value) {
    return dataMatcher.support(value);
  }

  public boolean matches(Object value) {
    return dataMatcher.matches(value);
  }

  public Collection<Class<?>> canCastClasses() {
    return dataMatcher.canCastClasses();
  }

  public <R> boolean canCast(Class<R> clazz) {
    return dataMatcher.canCast(clazz);
  }

  public <R> R cast(Object value, Class<R> clazz) {
    return dataMatcher.cast(value, clazz);
  }

  public ImmutableList<StandardCell> getDataCells(Menu menu) {
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

  public ConstructType getCastType() {
    return castType;
  }

  public boolean isBeList() {
    return beList;
  }
  
}
