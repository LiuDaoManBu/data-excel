package com.caotc.excel4j.matcher.data;

import java.util.Collection;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.data.type.DataType;

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
    return matchers.stream().allMatch(matcher -> matcher.support(value));
  }

  @Override
  public boolean matches(Object value) {
    return matchers.stream().allMatch(matcher -> matcher.matches(value));
  }

  public DataType getDataType() {
    return dataType;
  }

  public Collection<Matcher> getMatchers() {
    return matchers;
  }
  
}
