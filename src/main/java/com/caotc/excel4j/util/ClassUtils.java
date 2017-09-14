package com.caotc.excel4j.util;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ClassUtils extends org.apache.commons.lang3.ClassUtils {
  private ClassUtils() {
    throw new AssertionError();
  }

  public static Stream<Field> getAllFieldStream(Class<?> type) {
    List<Class<?>> classes = getAllSuperclasses(type);
    classes.add(type);
    return classes.stream().map(Class::getDeclaredFields).map(Arrays::asList)
        .flatMap(Collection::stream);
  }

  /**
   * get all fields of the Class
   * 
   * @param type Class Object
   * @return all fields of the Class
   */
  public static Collection<Field> getAllFields(Class<?> type) {
    return getAllFieldStream(type).collect(Collectors.toList());
  }

  public static Map<String, Field> getNameToFields(Class<?> type) {
    return getAllFieldStream(type).collect(Collectors.toMap(Field::getName, Function.identity(),
        (field1, field2) -> field1.getDeclaringClass().isAssignableFrom(field2.getDeclaringClass())
            ? field2
            : field1));
  }

  public static Field getField(Class<?> type, String fieldName) {
    Field field = null;
    if (type != null && fieldName != null) {
      Map<String, Field> nameToFields = getNameToFields(type);
      field = nameToFields.get(fieldName);
    }
    return field;
  }
}
