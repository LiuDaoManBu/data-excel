package com.caotc.excel4j.matcher.data.value;

import com.caotc.excel4j.matcher.data.type.NaturalDataType;

public abstract class StringMatcher extends DataValueMatcher {

  @Override
  public boolean support(Object value) {
    return NaturalDataType.STRING.matches(value);
  }

  @Override
  public boolean matches(Object value) {
    if (!support(value)) {
      throw new IllegalArgumentException(
          "this matcher can't matches this object " + value + " please check " + "matchers config");
    }
    return matches(NaturalDataType.STRING.cast(value, String.class));
  }

  public abstract boolean matches(String value);
}
