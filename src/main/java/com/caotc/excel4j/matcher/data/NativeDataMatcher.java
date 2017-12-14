package com.caotc.excel4j.matcher.data;

import java.util.Collection;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.Iterables;
import com.google.common.reflect.TypeToken;

public class NativeDataMatcher extends AbstractDataMatcher {
  private final Collection<DataMatcher> matchers;
  private final Predicate<Object> matcher;
  
  public NativeDataMatcher(DataType dataType, Collection<DataMatcher> matchers) {
    super(dataType);
    this.matchers = matchers;
//    matchers.stream().map(DataMatcher::test);
    matcher=null;
  }

  @Override
  public boolean test(Object value) {
    return Iterables.all(matchers, matcher -> matcher.test(value));
  }
  
  //delegate methods start
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

  //delegate methods end
  
  public DataType getDataType() {
    return dataType;
  }

  public Collection<DataMatcher> getMatchers() {
    return matchers;
  }
  
}
