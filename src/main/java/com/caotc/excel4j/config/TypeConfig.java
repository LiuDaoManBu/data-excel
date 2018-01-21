package com.caotc.excel4j.config;

import com.google.common.reflect.TypeToken;

public abstract class TypeConfig<T> {
  private TypeToken<T> type;

  public TypeConfig() {
    TypeToken<? extends TypeConfig> type = TypeToken.of(getClass());
    
  }

  public TypeToken<T> getType() {
    return type;
  }

  public void setType(TypeToken<T> type) {
    this.type = type;
  }
}
