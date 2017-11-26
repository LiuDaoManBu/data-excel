package com.caotc.excel4j;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.util.TypeUtils;
import com.caotc.excel4j.config.ParserConfig;
import com.caotc.excel4j.util.ClassUtil;
import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.reflect.AbstractInvocationHandler;
import com.google.common.reflect.Invokable;
import com.google.common.reflect.Reflection;
import com.google.common.reflect.TypeToken;

interface Table<T> {
  @SuppressWarnings("serial")
  public default T get() {
    TypeToken<T> typeToken = new TypeToken<T>(getClass()) {};
    System.out.println(typeToken);
    System.out.println(typeToken.getRawType());
    // TypeToken<?> token=typeToken.resolveType(typeToken.getRawType().getTypeParameters()[0]);
    // System.out.println(token);
    return null;
  }
}


class AAA<T> {
  public Set<T> values;

  @Override
  public String toString() {
    return "AAA [values=" + values + "]";
  }
}


class BBB<T> extends AAA<T> implements Table<T> {
  public final String name = "name";
  public String type;
}


class MyInvocationHandler<T> extends AbstractInvocationHandler {
  private Table<T> table;

  MyInvocationHandler(Table<T> table) {
    this.table = table;
  }

  @SuppressWarnings("serial")
  public T get() {
    TypeToken<T> typeToken = new TypeToken<T>(getClass()) {};
    System.out.println(typeToken);
    System.out.println(typeToken.getRawType());
    // TypeToken<?> token=typeToken.resolveType(typeToken.getRawType().getTypeParameters()[0]);
    // System.out.println(token);
    return null;
  }

  @Override
  protected Object handleInvocation(Object proxy, Method method, Object[] args) throws Throwable {
    get();
    return method.invoke(table, args);
  }

}


interface Ia {
}


interface Ib extends Ia {
}


public class TestGuava {
  public static <T> void main(String[] args) throws Exception {
    // Table<List<String>> table=new Table<List<String>>();
    // List<String> n=table.get();

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
    // System.out.println(int[][][].class.equals(int[].class));
    // Class<?> type=void.class;
    // System.out.println(type.isInterface());
    // System.out.println(type.isLocalClass());
    // System.out.println(type.isMemberClass());
    // System.out.println(type.isSynthetic());
    //
    // TypeToken token=TypeToken.of(void.class);
    // System.out.println(token.isPrimitive());
    Table<String> table = new BBB<String>();
    Table<String> proxy = Reflection.newProxy(Table.class, new MyInvocationHandler<String>(table));
    proxy.get();

    TypeToken<BBB> token = TypeToken.of(BBB.class);
    System.out.println(Arrays.asList(TestGuava.class.getConstructors()));

    testTypeToken(new TypeToken<Collection<String>>() {});

    Iterable<String> strings = Lists.newArrayList();
    System.out.println(strings instanceof Set);
  }

  public static <T> void testTypeToken(TypeToken<T> token) {
    System.out.println(token.resolveType(token.getRawType().getTypeParameters()[0]));
  }

  public static <T> void testInvokableCast() throws Exception {
    Invokable<?, Object> invokable = Invokable.from(TestGuava.class.getMethod("get"));
    System.out.println(invokable.invoke(null));
    Invokable<?, T> invokable2 = (Invokable<?, T>) invokable;
    System.out.println(invokable2);
    System.out.println(invokable2.invoke(null));
  }

  public static <T> T get() {
    return null;
  }
}
