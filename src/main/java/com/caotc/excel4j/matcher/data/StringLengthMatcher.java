package com.caotc.excel4j.matcher.data;

import com.caotc.excel4j.matcher.data.type.DataType;

public abstract class StringLengthMatcher extends StringMatcher {
  public StringLengthMatcher(DataType dataType) {
    super(dataType);
  }

  @Override
  public boolean testValue(String t) {
    return testValue(t.length());
  }

  public abstract boolean testValue(int length);
}
