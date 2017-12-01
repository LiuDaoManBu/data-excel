package com.caotc.excel4j.constant;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.ParserConfig;
import com.caotc.excel4j.parse.result.Data;
import com.caotc.excel4j.util.ClassUtil;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.reflect.TypeToken;

public enum ConstructType {
  OBJECT {
    @Override
    public <T> T construct(TypeToken<T> type, Map<String, Data<?>> nameToDatas) {
      ImmutableMultimap<String, Field> nameToFields = ClassUtil.getNameToFields(type);
      T target=ParserConfig.GLOBAL.newInstance(type).get();
      nameToDatas.forEach((name, data) -> {
        nameToFields.get(name).forEach(field -> {
          Class<?> filedType = field.getType();
          Object value = data.getValue().get();
          // TODO 判断泛型对象cast?
          if (data.getDataConfig().canCast(filedType)) {
            field.setAccessible(true);
            value = data.getDataConfig().cast(value, filedType);
            try {
              // TODO NULL?log?
              field.set(value, target);
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
    public <T> T construct(TypeToken<T> type, Map<String, Data<?>> nameToDatas) {
      Preconditions.checkArgument(ClassUtil.isArrayOrIterable(type));
      T target=ParserConfig.GLOBAL.newInstance(type).get();
      TypeToken<?> genericType = ClassUtil.getComponentOrGenericType(type);
      ImmutableMultimap<String, Field> nameToFields = ClassUtil.getNameToFields(genericType);
      Collection<? extends Object> result=Lists.newLinkedList();
      nameToDatas.forEach((name,data)->{
        
      });
      if(type.isArray()) {
      }
      
      if(target instanceof Collection) {
      }
      
      
      return target;
    }
  };
  private static final Splitter SPLITTER = Splitter.on(".").omitEmptyStrings();

  private static void setValue(JSONObject object, String name, Object value) {
    List<String> names = SPLITTER.splitToList(name);
    if (CollectionUtils.isNotEmpty(names)) {
      for (String n : names.subList(0, names.size() - 1)) {
        if (!object.containsKey(n)) {
          object.put(n, new JSONObject());
        }
        object = object.getJSONObject(n);
      }
      object.put(Iterables.getLast(names), value);
    }
  }

  private static <T> T[] toArray(Object o) {
    return (T[]) o;
  }

  public abstract <T> T construct(TypeToken<T> type, Map<String, Data<?>> nameToDatas);
}
