package com.caotc.excel4j.constant;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import com.caotc.excel4j.config.ParserConfig;
import com.caotc.excel4j.util.ClassUtil;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.reflect.TypeToken;

public enum ConstructType {
  OBJECT {
    @Override
    public <T> T construct(TypeToken<T> type, Map<String, ?> nameToValues,
        ParserConfig parserConfig) {
      ImmutableMultimap<String, Field> nameToFields = ClassUtil.getNameToFields(type);
      T target = parserConfig.newInstance(type).get();
      nameToValues.forEach((name, value) -> {
        nameToFields.get(name).forEach(field -> {
          field.setAccessible(true);
          Class<?> filedType = field.getType();
          // TODO 基本类型?
          if (filedType.isInstance(value)) {
            try {
              field.set(target, value);
            } catch (IllegalArgumentException | IllegalAccessException e) {
              e.printStackTrace();
            }
          }
        });
      });
      return target;
    }
  },
  ITERABLE {
    @Override
    public <T> T construct(TypeToken<T> type, Map<String, ?> nameToValues,
        ParserConfig parserConfig) {
      Preconditions.checkArgument(ClassUtil.isArrayOrIterable(type));
      // TODO value为Iterable或Array
      Preconditions
          .checkArgument(nameToValues.values().stream().allMatch(value -> value instanceof List));

      List<Map<String, Object>> params = Lists.newLinkedList();
      nameToValues.forEach((name, value) -> {
        List<?> values =
            value.getClass().isArray() ? Arrays.asList(toArray(value)) : (List<?>) value;
        for (int i = 0; i < values.size(); i++) {
          if (params.size() < i + 1) {
            params.add(Maps.newHashMap());
          }
          params.get(i).put(name, values.get(i));
        }
      });

      TypeToken<?> genericType = ClassUtil.getComponentOrGenericType(type);
      ConstructType genericConstructType = parserConfig.getConstructType(genericType);
      Stream<?> targets = params.stream()
          .map(param -> genericConstructType.construct(genericType, param, parserConfig));
      if (type.isArray()) {
        // TODO T为String[]等时,Object[]无法cast为String[]
        return (T) targets.toArray();
      }

      if (type.isSubtypeOf(TypeToken.of(Collection.class))) {
        Collection collection = (Collection) ParserConfig.GLOBAL.newInstance(type).get();
        Iterables.addAll(collection, targets.collect(Collectors.toList()));
        return (T) collection;
      }

      return (T) targets.collect(Collectors.toList());
    }
  };
  private static final Splitter SPLITTER = Splitter.on(".").omitEmptyStrings();

  private static <T> T[] toArray(Object value) {
    // TODO 基本类型无法cast为Object[]
    return (T[]) value;
  }

  public abstract <T> T construct(TypeToken<T> type, Map<String, ?> nameToValues,
      ParserConfig parserConfig);
}
