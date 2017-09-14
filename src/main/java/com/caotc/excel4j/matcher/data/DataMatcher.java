package com.caotc.excel4j.matcher.data;

import java.util.Collection;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.matcher.data.value.DataValueMatcher;
import com.caotc.excel4j.util.MatcherUtil;

public class DataMatcher implements Matcher {
  private DataType dataType;
  private Collection<Collection<DataValueMatcher>> dataValueMatchers;
  private Collection<Collection<? extends Matcher>> matchers;

  @Override
  public boolean support(Object value) {
    return matchers.stream().allMatch(matcher -> MatcherUtil.allSupport(matcher, value));
  }

  @Override
  public boolean matches(Object value) {
    return MatcherUtil.anyMatches(matchers, value);
  }

  public DataType getDataType() {
    return dataType;
  }

  public void setDataType(DataType dataType) {
    this.dataType = dataType;
  }

  public Collection<Collection<DataValueMatcher>> getDataValueMatchers() {
    return dataValueMatchers;
  }

  public void setDataValueMatchers(Collection<Collection<DataValueMatcher>> dataValueMatchers) {
    this.dataValueMatchers = dataValueMatchers;
  }

  public Collection<Collection<? extends Matcher>> getMatchers() {
    return matchers;
  }

  public void setMatchers(Collection<Collection<? extends Matcher>> matchers) {
    this.matchers = matchers;
  }
}
