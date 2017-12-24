package com.caotc.excel4j.matcher.data;

import com.caotc.excel4j.matcher.Matcher;

public interface DataMatcher extends Matcher<Object> {
  boolean support(Object value);
}
