package com.caotc.excel4j.matcher.data.type;

import java.util.Collection;
import com.caotc.excel4j.matcher.Matcher;



public interface DataType extends Matcher {
  @Override
  default boolean support(Object value) {
    return true;
  }

  abstract Collection<Class<?>> canCastClasses();

  abstract <T> boolean canCast(Class<T> clazz);

  abstract <T> T cast(Object value, Class<T> clazz);
}
