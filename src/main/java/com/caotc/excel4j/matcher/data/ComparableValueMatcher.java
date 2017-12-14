package com.caotc.excel4j.matcher.data;

import com.caotc.excel4j.matcher.data.type.DataType;

public abstract class ComparableValueMatcher<T extends Comparable<T>> extends BaseDataMatcher<T> {
  public ComparableValueMatcher(DataType dataType) {
    super(dataType);
  }

}
