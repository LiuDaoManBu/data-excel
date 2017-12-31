package com.caotc.excel4j.matcher.constant;

public enum ComparableMatcherType {
  //Less Than
  LT {
    @Override
    public <T extends Comparable<T>> boolean apply(T value, T predicateValue) {
      return value.compareTo(predicateValue) < 0;
    }
  },
  //Less Than or Equal
  LE {
    @Override
    public <T extends Comparable<T>> boolean apply(T value, T predicateValue) {
      return value.compareTo(predicateValue) <= 0;
    }
  },
  //Greater Than
  GT {
    @Override
    public <T extends Comparable<T>> boolean apply(T value, T predicateValue) {
      return value.compareTo(predicateValue) > 0;
    }
  },
  //Greater Than or Equal
  GE {
    @Override
    public <T extends Comparable<T>> boolean apply(T value, T predicateValue) {
      return value.compareTo(predicateValue) >= 0;
    }
  },
  //Equal
  EQ {
    @Override
    public <T extends Comparable<T>> boolean apply(T value, T predicateValue) {
      return predicateValue.compareTo(value) == 0;
    }
  },
  //Not Equal
  NE {
    @Override
    public <T extends Comparable<T>> boolean apply(T value, T predicateValue) {
      return predicateValue.compareTo(value) != 0;
    }
  };

  public abstract <T extends Comparable<T>> boolean apply(T  value, T predicateValue);
}
