package com.caotc.excel4j.util;

import java.lang.reflect.Field;
import java.util.Arrays;
import com.google.common.base.Optional;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimaps;
import com.google.common.reflect.TypeToken;

public class ClassUtils extends org.apache.commons.lang3.ClassUtils {
  private ClassUtils() {
    throw new AssertionError();
  }

  /**
   * get all fields of the Class.
   * 
   * @param type Class Object
   * @return all fields of the Class
   */
  public static ImmutableCollection<Field> getAllFields(Class<?> type) {
    return FluentIterable.from(TypeToken.of(type).getTypes().classes().rawTypes())
        .transform(Class::getDeclaredFields).transformAndConcat(Arrays::asList).toSet();
  }

  public static ImmutableMultimap<String, Field> getNameToFields(Class<?> type) {
    return Multimaps.index(getAllFields(type), Field::getName);
  }

  public static Optional<Field> getField(Class<?> type, String fieldName) {
    return Optional
        .fromNullable(Iterables.getOnlyElement(getNameToFields(type).get(fieldName), null));
  }
}
