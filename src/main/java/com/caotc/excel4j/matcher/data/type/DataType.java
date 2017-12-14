package com.caotc.excel4j.matcher.data.type;

import java.util.function.Predicate;
import com.google.common.collect.ImmutableCollection;
import com.google.common.reflect.TypeToken;



public interface DataType extends Predicate<Object> {
  abstract ImmutableCollection<TypeToken<?>> canCastTypes();
  
  default <T> boolean canCast(Class<T> type) {
    return canCast(TypeToken.of(type));
  }
  
  abstract <T> boolean canCast(TypeToken<T> type);

  default <T> T cast(Object value, Class<T> type) {
    return cast(value,TypeToken.of(type));
  }
  
  abstract <T> T cast(Object value, TypeToken<T> type);
}
