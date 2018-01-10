package com.caotc.excel4j.matcher.constant;

public enum StringMatcherType{
  EQUALS {
    @Override
    public boolean apply(String value, String predicateValue) {
      return value.equals(predicateValue);
    }
  },
  EQUALS_IGNORE_CASE {
    @Override
    public boolean apply(String value, String predicateValue) {
      return value.equalsIgnoreCase(predicateValue);
    }
  },
  CONTAINS {
    @Override
    public boolean apply(String value, String predicateValue) {
      return value.contains(predicateValue);
    }
  },
  MATCHES {
    @Override
    public boolean apply(String value, String predicateValue) {
      return value.matches(predicateValue);
    }
  },
  STARTS_WITH {
    @Override
    public boolean apply(String value, String predicateValue) {
      return value.startsWith(predicateValue);
    }
  },
  ENDS_WITH {
    @Override
    public boolean apply(String value, String predicateValue) {
      return value.endsWith(predicateValue);
    }
  };

  public abstract boolean apply(String value, String predicateValue);
}
