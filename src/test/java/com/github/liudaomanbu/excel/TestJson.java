package com.github.liudaomanbu.excel;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Collection;
import java.util.Map;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.util.TypeUtils;
import com.github.liudaomanbu.excel.matcher.constant.Type;
import com.google.auto.value.AutoValue;
import com.google.common.collect.Multimap;

class Data {
  
  private Class<?> type;
  private Type t;
  
  public Type getT() {
    return t;
  }

  public void setT(Type t) {
    this.t = t;
  }

  public Class<?> getType() {
    return type;
  }

  public void setType(Class<?> type) {
    this.type = type;
  }

}


public class TestJson {
  public static void main(String[] args) {
  }

  public static void testCastLocalDate() {
    System.out.println(TypeUtils.castToJavaBean(Calendar.getInstance().getTime(), LocalDate.class));
    System.out.println(TypeUtils.castToJavaBean("2000-6-25", LocalDate.class));
  }
  
  public static void testJsonToGuavaCollection() {
    JSONObject jsonObject = new JSONObject();
    JSONArray jsonArray = new JSONArray();
    jsonArray.fluentAdd(new JSONObject().fluentPut("name", "a").fluentPut("type", "region"))
        .fluentAdd(new JSONObject().fluentPut("name", "b").fluentPut("type", "city"));
    jsonObject.put("values", jsonArray);
    System.out.println(jsonObject.toJavaObject(AAA.class));
    Collection<AAA> c = new JSONArray().fluentAdd(jsonObject).toJavaObject(Collection.class);
    System.out.println(c);
    // AAA aaa=c.iterator().next();

    System.out.println(jsonObject.toJavaObject(Map.class).get("values"));
    Multimap<String, Object> mmap = jsonObject.toJavaObject(Multimap.class);
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
  
  public static void testEnum() {
    JSONObject jsonObject=new JSONObject();
    jsonObject.put("t", "OR");
    Data data=jsonObject.toJavaObject(Data.class);
    System.out.println(data.getT());
  }
}
