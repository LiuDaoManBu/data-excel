package com.caotc.excel4j.matcher.data.constant;

public enum StringMatcherType {
  EQUALS {
    @Override
    public boolean matches(String string, String matchString) {
      return string.equals(matchString);
    }
  },
  CONTAINS {
    @Override
    public boolean matches(String string, String matchString) {
      return string.contains(matchString);
    }
  },
  MATCHES {
    @Override
    public boolean matches(String string, String matchString) {
      return string.matches(matchString);
    }
  },
  STARTSWITH {
    @Override
    public boolean matches(String string, String matchString) {
      return string.startsWith(matchString);
    }
  },
  ENDSWITH {
    @Override
    public boolean matches(String string, String matchString) {
      return string.endsWith(matchString);
    }
  };

  public abstract boolean matches(String string, String matchString);
}
