package com.caotc.excel4j.matcher;

public interface Matcher {
  boolean support(Object value);

  boolean matches(Object value);
}
