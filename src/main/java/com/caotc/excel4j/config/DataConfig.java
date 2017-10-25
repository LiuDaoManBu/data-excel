package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.Collection;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.collect.ImmutableList;

public class DataConfig {
  private MenuConfig menuConfig;
  private Field field;
  private DataMatcher dataMatcher;
  private LoadType loadType;
  private Integer dataNumber;


  public boolean support(Object value) {
    return dataMatcher.support(value);
  }

  public boolean matches(Object value) {
    return dataMatcher.matches(value);
  }

  public Collection<Class<?>> canCastClasses() {
    return dataMatcher.canCastClasses();
  }

  public <T> boolean canCast(Class<T> clazz) {
    return dataMatcher.canCast(clazz);
  }

  public <T> T cast(Object value, Class<T> clazz) {
    return dataMatcher.cast(value, clazz);
  }

  public ImmutableList<StandardCell> getDataCells(Menu menu) {
    return loadType.getDataCells(menu);
  }


  public MenuConfig getMenuConfig() {
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
  
}
