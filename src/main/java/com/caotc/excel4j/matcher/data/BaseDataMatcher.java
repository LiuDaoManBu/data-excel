package com.caotc.excel4j.matcher.data;

import java.lang.reflect.TypeVariable;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.reflect.TypeToken;

public abstract class BaseDataMatcher<T> extends AbstractDataMatcher {
  private final TypeToken<T> type;
  
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public BaseDataMatcher(DataType dataType) {
    super(dataType);
    TypeToken<? extends BaseDataMatcher> token=TypeToken.of(getClass());
    TypeVariable<?>[] types=token.getSupertype(BaseDataMatcher.class).getRawType().getTypeParameters();
    type=TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
  }

  @Override
  public boolean test(Object t) {
    return testValue(dataType.cast(t, type));
  }

  public abstract boolean testValue(T t);
  
}
