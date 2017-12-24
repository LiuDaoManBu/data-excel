package com.caotc.excel4j.matcher.data;

import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.data.type.DataType;

public abstract class AbstractDataMatcher extends BaseMatcher<Object> implements DataMatcher {
  protected final DataType dataType;
  
  public AbstractDataMatcher(DataType dataType) {
    super();
    this.dataType = dataType;
  }

  public boolean support(Object value) {
    return dataType.test(value);
  }
}
