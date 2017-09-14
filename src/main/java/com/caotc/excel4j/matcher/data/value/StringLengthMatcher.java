package com.caotc.excel4j.matcher.data.value;

import com.caotc.excel4j.matcher.data.value.ComparableValueMatcher.ComparableValueMatcherType;

public class StringLengthMatcher extends StringMatcher {
  private static final ComparableValueMatcherType DEFAULT_COMPARABLE_VALUE_MATCHER_TYPE =
      ComparableValueMatcherType.MAX_OR_EQUALS;
  private ComparableValueMatcher<Integer> valueMatcher;

  public StringLengthMatcher() {
    super();
  }

  public StringLengthMatcher(ComparableValueMatcher<Integer> valueMatcher) {
    super();
    this.valueMatcher = valueMatcher;
  }

  public StringLengthMatcher(int matcherValue, ComparableValueMatcherType valueMatcherType) {
    this(new ComparableValueMatcher<Integer>(matcherValue, valueMatcherType));
  }

  public StringLengthMatcher(int matcherValue) {
    this(matcherValue, DEFAULT_COMPARABLE_VALUE_MATCHER_TYPE);
  }

  public ComparableValueMatcher<Integer> getValueMatcher() {
    return valueMatcher;
  }

  public void setValueMatcher(ComparableValueMatcher<Integer> valueMatcher) {
    this.valueMatcher = valueMatcher;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((valueMatcher == null) ? 0 : valueMatcher.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    StringLengthMatcher other = (StringLengthMatcher) obj;
    if (valueMatcher == null) {
      if (other.valueMatcher != null)
        return false;
    } else if (!valueMatcher.equals(other.valueMatcher))
      return false;
    return true;
  }

  @Override
  public boolean matches(String value) {
    return valueMatcher.matches(value.length());
  }
}
