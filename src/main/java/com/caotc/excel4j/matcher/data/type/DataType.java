package com.caotc.excel4j.matcher.data.type;

import java.util.function.Predicate;
import com.google.common.collect.ImmutableCollection;
import com.google.common.reflect.TypeToken;



public interface DataType extends Predicate<Object> {
  abstract ImmutableCollection<TypeToken<?>> canCastTypes();
  
  abstract <T> boolean canCast(Class<T> type);
  
  abstract <T> boolean canCast(TypeToken<T> type);

  abstract <T> T cast(Object value, Class<T> type);
  
  abstract <T> T cast(Object value, TypeToken<T> type);
}
