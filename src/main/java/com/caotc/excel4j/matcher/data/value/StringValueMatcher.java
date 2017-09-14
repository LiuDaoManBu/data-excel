package com.caotc.excel4j.matcher.data.value;

public class StringValueMatcher extends StringMatcher {
  private static final StringValueMatcherType DEFAULT_STRING_VALUE_MATCHER_TYPE =
      StringValueMatcherType.EQUALS;
  private String matchString;
  private StringValueMatcherType stringMatchType;

  public StringValueMatcher() {
    super();
  }

  public StringValueMatcher(String matchString) {
    this(matchString, DEFAULT_STRING_VALUE_MATCHER_TYPE);
  }

  public StringValueMatcher(String matchString, StringValueMatcherType stringMatchType) {
    super();
    this.matchString = matchString;
    this.stringMatchType = stringMatchType;
  }

  public boolean matches(String string) {
    return stringMatchType.matches(string, matchString);
  }

  public String getMatchString() {
    return matchString;
  }

  public void setMatchString(String matchString) {
    this.matchString = matchString;
  }

  public StringValueMatcherType getStringMatchType() {
    return stringMatchType;
  }

  public void setStringMatchType(StringValueMatcherType stringMatchType) {
    this.stringMatchType = stringMatchType;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((matchString == null) ? 0 : matchString.hashCode());
    result = prime * result + ((stringMatchType == null) ? 0 : stringMatchType.hashCode());
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
    StringValueMatcher other = (StringValueMatcher) obj;
    if (matchString == null) {
      if (other.matchString != null)
        return false;
    } else if (!matchString.equals(other.matchString))
      return false;
    if (stringMatchType != other.stringMatchType)
      return false;
    return true;
  }

  public static enum StringValueMatcherType {
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
}
