package com.caotc.excel4j.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.GlobalConfig;
import com.google.common.base.Suppliers;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Table;
import com.google.common.reflect.Invokable;
import com.google.common.reflect.TypeToken;

public class ClassUtil extends org.apache.commons.lang3.ClassUtils {
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
        .ofNullable(Iterables.getOnlyElement(getNameToFields(type).get(fieldName), null));
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
    return (T) INTERFACE_TO_SUPPLIERS.get(Iterables.getFirst(interfaces, null)).get();
  }

  public static <T> Optional<Supplier<T>> getSupplier(Class<T> type) {
    Optional<Supplier<T>> optional = GlobalConfig.getSupplier(type);
    if (optional.isPresent()) {
      return optional;
    }
    return getDefaultConstructor(type).map(c -> new Supplier<T>() {
      @Override
      public T get() {
        c.setAccessible(Boolean.TRUE);
        return c.newInstance();
      }

    });
  }

  @SuppressWarnings("unchecked")
  public static <T> Optional<Constructor<T>> getDefaultConstructor(Class<T> type) {
    return Arrays.stream(type.getDeclaredConstructors())
        .filter(constructor -> constructor.getParameterTypes().length == 0).findAny()
        .map(constructor -> (Constructor<T>) constructor);// TODO

    // TODO
    // if (defaultConstructor == null) {
    // if (type.isMemberClass() && !Modifier.isStatic(type.getModifiers())) {
    // Class<?>[] types;
    // for (Constructor<?> constructor : constructors) {
    // if ((types = constructor.getParameterTypes()).length == 1
    // && types[0].equals(type.getDeclaringClass())) {
    // defaultConstructor = (Constructor<T>) constructor;
    // break;
    // }
    // }
    // }
    // }
  }

  private ClassUtil() {
    throw new AssertionError();
  }
}
