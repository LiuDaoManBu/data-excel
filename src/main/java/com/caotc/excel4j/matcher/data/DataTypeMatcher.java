package com.caotc.excel4j.matcher.data;

import java.lang.reflect.TypeVariable;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.reflect.TypeToken;

public abstract class DataTypeMatcher<T> extends BaseDataMatcher implements DataMatcher {
  private final DataType dataType;
  private final TypeToken<T> type;

  @SuppressWarnings({"rawtypes", "unchecked"})
  public DataTypeMatcher(DataType dataType) {
    this.dataType = dataType;
    TypeToken<? extends DataTypeMatcher> token = TypeToken.of(getClass());
    TypeVariable<?>[] types =
        token.getSupertype(DataTypeMatcher.class).getRawType().getTypeParameters();
    type = TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
  }

  @Override
  public boolean support(Object value) {
    return dataType.test(value);
  }

  @Override
  public boolean test(Object t) {
    return testValue(dataType.cast(t, type));
  }

  public abstract boolean testValue(T t);

}
