package com.github.liudaomanbu.excel.matcher;

import java.util.function.Function;
import com.github.liudaomanbu.excel.matcher.constant.StringMatcherType;

public class StringMatcher extends BaseMatcher<String> {

  public StringMatcher add(StringMatcherType type, String predicateValue) {
    add(type, predicateValue, Function.identity());
    return this;
  }

  public StringMatcher equals(String predicateValue) {
    add(StringMatcherType.EQUALS, predicateValue);
    return this;
  }

  public StringMatcher equalsIgnoreCase(String predicateValue) {
    add(StringMatcherType.EQUALS_IGNORE_CASE, predicateValue);
    return this;
  }

  public StringMatcher contains(String predicateValue) {
    add(StringMatcherType.CONTAINS, predicateValue);
    return this;
  }

  public StringMatcher matches(String predicateValue) {
    add(StringMatcherType.MATCHES, predicateValue);
    return this;
  }

  public StringMatcher startsWith(String predicateValue) {
    add(StringMatcherType.STARTS_WITH, predicateValue);
    return this;
  }

  public StringMatcher endsWith(String predicateValue) {
    add(StringMatcherType.ENDS_WITH, predicateValue);
    return this;
  }
}
