package com.caotc.excel4j.config;

import com.caotc.excel4j.matcher.data.DataMatcher;

public class DataMatcherConfig<T extends Comparable<T>> {
  private Boolean nullAllow;
  private T maxValue;
  private T minValue;
  private Integer maxLength;
  private Integer minLength;

  public Boolean getNullAllow() {
    return nullAllow;
  }

  public void setNullAllow(Boolean nullAllow) {
    this.nullAllow = nullAllow;
  }

  public T getMaxValue() {
    return maxValue;
  }

  public void setMaxValue(T maxValue) {
    this.maxValue = maxValue;
  }

  public T getMinValue() {
    return minValue;
  }

  public void setMinValue(T minValue) {
    this.minValue = minValue;
  }

  public Integer getMaxLength() {
    return maxLength;
  }

  public void setMaxLength(Integer maxLength) {
    this.maxLength = maxLength;
  }

  public Integer getMinLength() {
    return minLength;
  }

  public void setMinLength(Integer minLength) {
    this.minLength = minLength;
  }
}
