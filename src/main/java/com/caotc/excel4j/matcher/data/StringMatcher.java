package com.caotc.excel4j.matcher.data;

import com.caotc.excel4j.matcher.data.type.DataType;

public abstract class StringMatcher extends BaseDataMatcher<String> {
  public StringMatcher(DataType dataType) {
    super(dataType);
  }
}
