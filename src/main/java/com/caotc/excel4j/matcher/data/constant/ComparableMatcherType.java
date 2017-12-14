package com.caotc.excel4j.matcher.data.constant;

public enum ComparableMatcherType {
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
  private static final int ZERO = 0;

  public abstract <T extends Comparable<T>> boolean matches(T matcherValue, T value);
}
