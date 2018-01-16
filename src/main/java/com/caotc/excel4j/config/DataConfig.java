package com.caotc.excel4j.config;

import com.google.common.reflect.TypeToken;

public class DataConfig<V> {
  public static class Builder<V> {
    private TypeToken<V> type;

    public TypeToken<V> getType() {
      return type;
    }

    public Builder<V> setType(TypeToken<V> type) {
      this.type = type;
      return this;
    }

  }

  private final TypeToken<V> type;

  protected DataConfig(Builder<V> builder) {
    this.type = builder.type;
  }

  public TypeToken<V> getType() {
    return type;
  }

}
