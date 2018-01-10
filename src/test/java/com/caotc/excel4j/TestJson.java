package com.caotc.excel4j;

import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.matcher.ComparableMatcher;
import com.caotc.excel4j.matcher.constant.Type;
import com.google.common.reflect.TypeToken;

class Data {
  private Class<?> type;

  public Class<?> getType() {
    return type;
  }

  public void setType(Class<?> type) {
    this.type = type;
  }

  @Override
  public String toString() {
    return "Data [type=" + type + "]";
  }

}


public class TestJson {
  public static void main(String[] args) {
    testClass();
  }

  public static void testClass() {
    Data data = new Data();
    data.setType(String.class);
    JSONObject jsonObject=(JSONObject) JSONObject.toJSON(data);
    System.out.println(jsonObject);
    System.out.println(jsonObject.toJavaObject(Data.class));
  }

  public static void testGenericType() {
    JSONObject typeToValuesJson = new JSONObject();
    typeToValuesJson.put("LT", 1);
//    typeToValuesJson.put("LE", 9.9);
    typeToValuesJson.put("GE", "1");
//    typeToValuesJson.put("GT", "1.0");
//    typeToValuesJson.put("NE", "8.8");
    JSONObject jsonObject = new JSONObject();
    jsonObject.put("typeToValues", typeToValuesJson);
  }
}
