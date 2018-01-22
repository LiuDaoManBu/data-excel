package com.github.liudaomanbu.excel.config;

import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import com.google.common.reflect.TypeToken;

public abstract class TypeConfig<T> {
  private TypeToken<T> type;

  @SuppressWarnings({ "unchecked", "rawtypes" })
  public TypeConfig() {
    TypeToken<? extends TypeConfig> token=TypeToken.of(getClass());
    System.out.println(token);
    TypeVariable<?>[] types=token.getSupertype(TypeConfig.class).getRawType().getTypeParameters();
    System.out.println(Arrays.toString(types));
    type=TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
    System.out.println(type);
  }

  public TypeToken<T> getType() {
    return type;
  }

  public void setType(TypeToken<T> type) {
    this.type = type;
  }
}
