package com.caotc.excel4j.matcher.data;

import com.caotc.excel4j.matcher.data.type.DataType;

public abstract class ComparableMatcher<T extends Comparable<T>> extends BaseDataMatcher<T> {

  public ComparableMatcher(DataType dataType) {
    super(dataType);
  }

}
