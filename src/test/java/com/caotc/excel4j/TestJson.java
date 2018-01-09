package com.caotc.excel4j;

import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.matcher.ComparableMatcher;
import com.caotc.excel4j.matcher.constant.Type;
import com.google.common.reflect.TypeToken;

class Data {
  private Type type;

  public Type getType() {
    return type;
  }

  public void setType(Type type) {
    this.type = type;
  }

  @Override
  public String toString() {
    return "Data [type=" + type + "]";
  }

}


public class TestJson {
  public static void main(String[] args) {
    testGenericType();
  }

  public static void testEnum() {
    JSONObject jsonObject = new JSONObject();
    jsonObject.put("type", "OR");
    Data data = jsonObject.toJavaObject(Data.class);
    System.out.println(data);
  }

  private static void testGenericType() {
    JSONObject typeToValuesJson = new JSONObject();
    typeToValuesJson.put("LT", 1);
//    typeToValuesJson.put("LE", 9.9);
    typeToValuesJson.put("GE", "1");
//    typeToValuesJson.put("GT", "1.0");
//    typeToValuesJson.put("NE", "8.8");
    JSONObject jsonObject = new JSONObject();
    jsonObject.put("typeToValues", typeToValuesJson);
    ComparableMatcher.Builder<Integer> builder =
        jsonObject.toJavaObject(new TypeToken<ComparableMatcher.Builder<Integer>>(){}.getType());
    System.out.println(builder.getTypeToValues());
    builder.getTypeToValues().forEach((type,value)->{
      System.out.println(type);
      System.out.println(value.compareTo(5));
    });
  }
}
