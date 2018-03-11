package com.github.liudaomanbu.excel;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.github.liudaomanbu.excel.annotation.ExcelField;
import com.github.liudaomanbu.excel.annotation.ExcelMenu;
import com.github.liudaomanbu.excel.constant.ConstructType;
import com.github.liudaomanbu.excel.matcher.ComparableMatcher;
import com.github.liudaomanbu.excel.matcher.data.DataTypeMatcher;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.Sets;
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


class User {
  public final String a;
  @ExcelField(menus = @ExcelMenu(""))
  private String userName;
  private String passWord;
  private String region;

  public User() {
    a = null;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public String getPassWord() {
    return passWord;
  }

  public void setPassWord(String passWord) {
    this.passWord = passWord;
  }

  public String getRegion() {
    return region;
  }

  public void setRegion(String region) {
    this.region = region;
  }

}


public class Test {
  public static void main(String[] args) throws Exception {
    testLambda();
  }

  // public static void testParse() {
  // String path1 = "C:\\Users\\呵呵\\Desktop\\用户.xlsx";
  // String path2 = "C:\\Users\\Administrator\\Desktop\\用户.xlsx";
  //
  // MenuDataConfig.Builder userNameDataConfig =
  // MenuDataConfig.builder().setDataType(BaseDataType.STRING).setLoadType(LoadType.UNFIXED)
  // .setField(User.class.getDeclaredField("userName"));
  //
  // MenuConfig.Builder userNameMenuConfig = MenuConfig.builder()
  // .setMatcherBuilder(
  // new StandardCellMatcher().addDataPredicate(StringMatcherType.EQUALS, "用户名"))
  // .setDataConfigBuilder(userNameDataConfig).setDirection(Direction.BOTTOM);
  //
  // WorkbookParseResult workbookParseResult = ExcelUtil.parse(new File(path1),
  // WorkbookConfig.builder()
  // .setSheetConfigBuilders(ImmutableList.of(SheetConfig.builder()
  // .setMatcherBuilder(new SheetMatcher().setType(Type.AND).setPredicates(
  // Lists.newArrayList(sheet -> sheet.getSheetName().equalsIgnoreCase("user"))))
  // .setTableConfigBuilders(ImmutableList.of(TableConfig.builder()
  // .setTopMenuConfigBuilders(ImmutableList.of(userNameMenuConfig))))))
  // .build());
  //
  //// workbookParseResult.getSheetParseResults().stream().map(SheetParseResult::getTables)
  //// .flatMap(Collection::stream).flatMap(com.caotc.excel4j.parse.result.Table::getDataMenus)
  //// .map(Menu::getData).map(com.caotc.excel4j.parse.result.MenuData::getErrors)
  //// .forEach(values -> values.forEach(error -> System.out.println(error.getMessage())));
  // }

  public static void testLambda() {
    DataTypeMatcher matcher = new DataTypeMatcher().setDataType(BaseDataType.NUMBER);
    Predicate<Object> predicate = t -> matcher.getDataType().test(t);
    matcher.setDataType(null);
    System.out.println(predicate.test(5));
  }
  
  public static void testConstructType() {
    Object array = new String[] {"AAA", "BBB", "CCC"};
    // Object array = new int[] {2, 3, 1};
//    String[] strings = ConstructType.toArray(array);
//    System.out.println(Arrays.toString(strings));
    // Array.class.convert(array);
  }

  public static void testGeneric() {
    ComparableMatcher<Integer> matcher = new ComparableMatcher<>();
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
