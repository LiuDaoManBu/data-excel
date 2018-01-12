package com.caotc.excel4j;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.ComparableMatcher;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.Sets;
import com.google.common.collect.Table;
import com.google.common.reflect.Invokable;

class A implements Cloneable {
  public static String test = "test";
  private String id = null;
  private Set<Integer> values;

  public String getId() {
    System.out.println("getId");
    return id;
  }

  public void setId(String id) {
    System.out.println("setId");
    this.id = id;
  }

  public Set<Integer> getValues() {
    return values;
  }

  public void setValues(Set<Integer> values) {
    this.values = values;
  }

  @Override
  public String toString() {
    return "A [id=" + id + ", values=" + values + "]";
  }

  public void test() {
    System.out.println("A test");
  }
}


class B<T> extends A {
  private final int id;
  private final Collection<Integer> values;
  private T[] t;

  public B() {
    super();
    this.id = 1111;
    values = null;
  }

  public T[] getT() {
    return t;
  }

  public void setT(T[] t) {
    this.t = t;
  }

  @Override
  public String toString() {
    return "B [id=" + id + ", values=" + values + "]";
  }

  public void test() {
    System.out.println("B test");
    super.test();
  }

}


class C {
  private Map<String, Integer> map;

  public Map<String, Integer> getMap() {
    return map;
  }

  public void setMap(Map<String, Integer> map) {
    this.map = map;
  }


}


public class Test {
  public static void main(String[] args) throws Exception {}

  public static void testGeneric() {
    ComparableMatcher<Integer> matcher = ComparableMatcher.<Integer>builder().build();
    System.out.println(matcher);
  }

  public static void testInvokeAbleGenericType() throws Exception {
    Invokable<?, Object> invokable = Invokable.from(Test.class.getDeclaredMethod("testInvokeAble",
        int.class, long.class, String.class, Object.class, Object.class, Object.class));
    System.out.println(Arrays.asList(invokable.getTypeParameters()));
  }

  public static <T, R> void testInvokeAble(int a, long b, String c, R r, T t1, T T2) {
    System.out.println("testInvokeAble");
  }

  public static void testGenericType() throws Exception {
    B<String> b = new B<String>();
    Field tField = b.getClass().getDeclaredField("t");
    System.out.println(tField);
    System.out.println(tField.getType());
    System.out.println(tField.getGenericType());
    Method method = b.getClass().getDeclaredMethod("setT", Object[].class);

  }

  public static <T> T getValue(Class<T> type) {
    Object value = null;
    if (value != null && !value.getClass().equals(type)) {
      value = BaseDataType.DECIMAL.cast(value, type);
    }
    return (T) value;
  }

  public static Set<Integer> test1(A a) {
    System.out.println(a.getValues().contains(1));
    return Sets.newHashSet(1);
  }

  public static void testJson() throws Exception {
    new A();
    // JSONObject jsonObject=new JSONObject();
    // jsonObject.put("id", "1");
    // System.out.println(JSONObject.toJavaObject(jsonObject, A.class));
    B<?> b = new B<Object>();
    System.out.println(b);
    Field field = B.class.getDeclaredField("values");
    field.setAccessible(true);
    field.set(b, Lists.newArrayList("a", "b"));
    System.out.println(b);
    field.set(b, Lists.newArrayList(111, 123));
    System.out.println(b);
    b.test();
    // Integer first=b.getValues().iterator().next();
    // System.out.println(first);
    // System.out.println(ClassUtils.getAllFields(B.class).stream().collect(Collectors.toMap(Field::getName,
    // Function.identity())));
    // JSONObject jsonObject=new JSONObject();
    // jsonObject.put("value1", 1);
    // jsonObject.put("value2", 2);
    // jsonObject.put("value3", 3);
    // jsonObject.forEach((key,value)->{jsonObject.put(key, (int)value*10);});
    // System.out.println(jsonObject);
    // Class<?> type=null;
    // System.out.println(getValue(type));

    System.out.println();
    JSONObject jsonObject = new JSONObject();
    JSONArray jsonArray = new JSONArray();
    System.out.println("JSONObject start");
    // System.out.println(jsonObject.toJavaObject(Collection.class));
    // System.out.println(jsonObject.toJavaObject(List.class));
    System.out.println(jsonObject.toJavaObject(Set.class));
    System.out.println(jsonObject.toJavaObject(Map.class));
    System.out.println(jsonObject.toJavaObject(Multiset.class));
    System.out.println(jsonObject.toJavaObject(Multimap.class));
    System.out.println(jsonObject.toJavaObject(Table.class));
    System.out.println("JSONObject end");

    System.out.println("JSONArray start");
    System.out.println(jsonArray.toJavaObject(Collection.class));
    System.out.println(jsonArray.toJavaObject(List.class));
    // System.out.println(jsonArray.toJavaObject(Set.class));
    // System.out.println(jsonArray.toJavaObject(Map.class));
    // System.out.println(jsonArray.toJavaObject(Multimap.class));
    // System.out.println(jsonArray.toJavaObject(Table.class));
    System.out.println("JSONArray end");
  }
}
