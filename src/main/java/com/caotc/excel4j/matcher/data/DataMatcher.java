package com.caotc.excel4j.matcher.data;

import java.util.Collection;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.collect.Iterables;

public class DataMatcher implements Matcher {
  private final DataType dataType;
  private final Collection<Matcher> matchers;

  
  public DataMatcher(DataType dataType, Collection<Matcher> matchers) {
    super();
    this.dataType = dataType;
    this.matchers = matchers;
  }

  @Override
  public boolean support(Object value) {
    return Iterables.all(matchers, matcher -> matcher.support(value));
  }

  @Override
  public boolean matches(Object value) {
    return Iterables.all(matchers, matcher -> matcher.matches(value));
  }
  
  //delegate methods start
  public Collection<Class<?>> canCastClasses() {
    return dataType.canCastClasses();
  }

  public <T> boolean canCast(Class<T> clazz) {
    return dataType.canCast(clazz);
  }

  public <T> T cast(Object value, Class<T> clazz) {
    return dataType.cast(value, clazz);
  }
  //delegate methods end
  
  public DataType getDataType() {
    return dataType;
  }

  public Collection<Matcher> getMatchers() {
    return matchers;
  }
  
}
