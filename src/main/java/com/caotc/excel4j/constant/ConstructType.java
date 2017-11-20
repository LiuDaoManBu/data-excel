package com.caotc.excel4j.constant;

import java.util.Map;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.google.common.base.Splitter;

public enum ConstructType {
  OBJECT {
    @Override
    public JSON construct(Map<String, ?> fieldNameToValues) {
      JSONObject object = new JSONObject();
      fieldNameToValues.forEach((name, value) -> {
        SPLITTER.split(name);
      });
      return object;
    }
  },
  COLLECTION {

    @Override
    public JSON construct(Map<String, ?> fieldNameToValues) {
      // TODO Auto-generated method stub
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
  private static final Splitter SPLITTER = Splitter.on(".");

  private static void setValue(JSONObject object, String name, Object value) {
    for (String n : SPLITTER.split(name)) {
      object = object.getJSONObject(n);
    }

  }

  public abstract JSON construct(Map<String, ?> fieldNameToValues);
}
