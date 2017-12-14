package com.caotc.excel4j.matcher.data;

import java.util.function.Predicate;

public interface DataMatcher extends Predicate<Object> {
  boolean support(Object value);
}
