package com.caotc.excel4j.util;

import java.awt.List;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import org.apache.poi.ss.formula.functions.T;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.base.Optional;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Table;
import com.google.common.reflect.Invokable;
import com.google.common.reflect.TypeToken;

public class ClassUtils extends org.apache.commons.lang3.ClassUtils {
//  private static final ImmutableMap<Class<T>, Invokable<T, T>> map=ImmutableMap.of(Collection.class, Invokable
//      .from(ArrayList.class.getConstructor()),List.class,Invokable
//      .from(ArrayList.class.getConstructor()));
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

  public static <T> boolean isSingle(Class<T> type) {
    TypeToken<T> token = TypeToken.of(type);
    return !(token.isArray() || token.isSubtypeOf(Collection.class) || token.isSubtypeOf(Map.class)
        || token.isSubtypeOf(Multimap.class) || token.isSubtypeOf(Table.class));
  }
  
  public static <T> T newInstance(Class<T> type) {
    if (isSingle(type)) {
      return new JSONArray().toJavaObject(type);
    }else {
      return new JSONObject().toJavaObject(type);
    }
  }
  
  private ClassUtils() {
    throw new AssertionError();
  }
}
