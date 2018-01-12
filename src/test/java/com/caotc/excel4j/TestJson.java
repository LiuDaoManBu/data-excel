package com.caotc.excel4j;

import java.sql.Date;
import java.time.LocalDate;
import java.util.Calendar;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.util.TypeUtils;

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
    testCastLocalDate();
  }

  public static void testCastLocalDate() {
    System.out.println(TypeUtils.castToJavaBean(Calendar.getInstance().getTime(), LocalDate.class));
    System.out.println(TypeUtils.castToJavaBean("2000-6-25", LocalDate.class));
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
