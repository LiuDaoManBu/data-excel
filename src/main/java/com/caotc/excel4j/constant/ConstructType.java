package com.caotc.excel4j.constant;

import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;

public enum ConstructType {
  OBJECT {
    @Override
    public JSON construct(Map<String, ?> fieldNameToValues) {
      JSONObject object = new JSONObject();
      fieldNameToValues.forEach((name, value) -> setValue(object, name, value));
      return object;
    }
  },
  COLLECTION {
    @Override
    public JSON construct(Map<String, ?> fieldNameToValues) {
      JSONArray array=new JSONArray();
      fieldNameToValues.forEach((name, value) -> {
        
      });
      return null;
    }
  },
  MAP {

    @Override
    public JSON construct(Map<String, ?> fieldNameToValues) {
      // TODO Auto-generated method stub
      return null;
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

  public abstract JSON construct(Map<String, ?> fieldNameToValues);
}
