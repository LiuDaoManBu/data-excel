package com.github.liudaomanbu.excel.config;

import java.lang.reflect.TypeVariable;
import com.google.common.reflect.TypeToken;

public abstract class TypeConfig<T> {
  private TypeToken<T> type;

  @SuppressWarnings({ "unchecked", "rawtypes" })
  public TypeConfig() {
    TypeToken<? extends TypeConfig> token=TypeToken.of(getClass());
    TypeVariable<?>[] types=token.getSupertype(TypeConfig.class).getRawType().getTypeParameters();
    type=TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
  }

  public TypeToken<T> getType() {
    return type;
  }

  public void setType(TypeToken<T> type) {
    this.type = type;
  }
}
