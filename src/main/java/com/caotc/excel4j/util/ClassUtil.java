package com.caotc.excel4j.util;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import com.alibaba.fastjson.JSONObject;
import com.google.common.base.Optional;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Ordering;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;
import com.google.common.reflect.TypeToken;

public class ClassUtil extends org.apache.commons.lang3.ClassUtils {
  private static final ImmutableMap<Class<?>, Supplier<?>> INTERFACE_TO_SUPPLIERS =
      ImmutableMap.of(List.class, Lists::newArrayList, Set.class, Sets::newHashSet,
          Collection.class, Lists::newArrayList, Multimap.class, ArrayListMultimap::create,
          Table.class, HashBasedTable::create);

  /**
   * get all fields of the Class.
   * 
   * @param <T>
   * 
   * @param type Class Object
   * @return all fields of the Class
   */
  public static <T> ImmutableCollection<Field> getAllFields(Class<?> type) {
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

  @SuppressWarnings("unchecked")
  public static <T> T newInstance(Class<T> type) {
    if (isSingle(type) || TypeToken.of(type).isSubtypeOf(Map.class)) {
      return new JSONObject().toJavaObject(type);
    }
    if (INTERFACE_TO_SUPPLIERS.containsKey(type)) {
      // TODO
      return (T) INTERFACE_TO_SUPPLIERS.get(type).get();
    }

    ImmutableList<Class<?>> interfaces = FluentIterable.from(INTERFACE_TO_SUPPLIERS.keySet())
        .filter(key -> TypeToken.of(key).isSupertypeOf(type)).toSortedList((left, right) -> {
          TypeToken<?> leftToken = TypeToken.of(left);
          if (leftToken.isSubtypeOf(right)) {
            return -1;
          }
          if (leftToken.isSupertypeOf(right)) {
            return 1;
          }
          return 0;
        });
    // TODO
    return (T) INTERFACE_TO_SUPPLIERS.get(Iterables.getFirst(interfaces, null)).get();
  }

  private ClassUtil() {
    throw new AssertionError();
  }
}
