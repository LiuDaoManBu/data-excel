package com.caotc.excel4j.matcher.data.value;

import com.caotc.excel4j.matcher.data.type.NaturalDataType;

public class ComparableValueMatcher<T extends Comparable<T>> extends DataValueMatcher {
  private static final ComparableValueMatcherType DEFAULT_COMPARABLE_VALUE_MATCHER_TYPE =
      ComparableValueMatcherType.MAX_OR_EQUALS;
  private static final int ZERO = 0;
  private T matcherValue;
  private ComparableValueMatcherType valueMatcherType;

  private ComparableValueMatcher() {
    super();
  }

  public ComparableValueMatcher(T matcherValue, ComparableValueMatcherType valueMatcherType) {
    this();
    this.matcherValue = matcherValue;
    this.valueMatcherType = valueMatcherType;
  }

  public ComparableValueMatcher(T matcherValue) {
    this(matcherValue, DEFAULT_COMPARABLE_VALUE_MATCHER_TYPE);
  }

  public T getMatcherValue() {
    return matcherValue;
  }

  public void setMatcherValue(T matcherValue) {
    this.matcherValue = matcherValue;
  }

  public ComparableValueMatcherType getValueMatcherType() {
    return valueMatcherType;
  }

  public void setValueMatcherType(ComparableValueMatcherType valueMatcherType) {
    this.valueMatcherType = valueMatcherType;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((matcherValue == null) ? 0 : matcherValue.hashCode());
    result = prime * result + ((valueMatcherType == null) ? 0 : valueMatcherType.hashCode());
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
    ComparableValueMatcher<?> other = (ComparableValueMatcher<?>) obj;
    if (matcherValue == null) {
      if (other.matcherValue != null)
        return false;
    } else if (!matcherValue.equals(other.matcherValue))
      return false;
    if (valueMatcherType != other.valueMatcherType)
      return false;
    return true;
  }

  @Override
  public boolean support(Object value) {
    return value instanceof Comparable || NaturalDataType.NUMBER.canCast(matcherValue.getClass());
  }

  @SuppressWarnings("unchecked")
  @Override
  public boolean matches(Object value) {
    if (!support(value)) {
      throw new IllegalArgumentException(
          "this matcher can't matches this object " + value + " please check " + "matchers config");
    }
    return valueMatcherType.matches(matcherValue,
        (T) NaturalDataType.NUMBER.cast(value, matcherValue.getClass()));
  }

  public static enum ComparableValueMatcherType {
    MAX {
      @Override
      public <T extends Comparable<T>> boolean matches(T matcherValue, T value) {
        return matcherValue.compareTo(value) > ZERO;
      }
    },
    MAX_OR_EQUALS {
      @Override
      public <T extends Comparable<T>> boolean matches(T matcherValue, T value) {
        return matcherValue.compareTo(value) >= ZERO;
      }
    },
    MIN {
      @Override
      public <T extends Comparable<T>> boolean matches(T matcherValue, T value) {
        return matcherValue.compareTo(value) < ZERO;
      }
    },
    MIN_OR_EQUALS {
      @Override
      public <T extends Comparable<T>> boolean matches(T matcherValue, T value) {
        return matcherValue.compareTo(value) <= ZERO;
      }
    },
    EQUALS {
      @Override
      public <T extends Comparable<T>> boolean matches(T matcherValue, T value) {
        return matcherValue.compareTo(value) == ZERO;
      }
    };
    public abstract <T extends Comparable<T>> boolean matches(T matcherValue, T value);
  }
}
