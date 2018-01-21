package com.github.liudaomanbu.excel.matcher.data.type;

import java.util.function.Predicate;
import com.google.common.reflect.TypeToken;



public interface DataType extends Predicate<Object> {
  <T> boolean canCast(Class<T> type);
  
  <T> boolean canCast(TypeToken<T> type);

  <T> T cast(Object value, Class<T> type);
  
  <T> T cast(Object value, TypeToken<T> type);
}
