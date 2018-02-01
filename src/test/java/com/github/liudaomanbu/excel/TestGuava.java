package com.github.liudaomanbu.excel;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
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


class FieldTest {
  public Class<?> type;
  public Field field;

  public FieldTest(Field field) {
    this.field = field;
    this.type = field.getType();
  }

  @Override
  public String toString() {
    return "FieldTest [type=" + type + ", field=" + field + "]";
  }

}


public class TestGuava {
  public static <T> void main(String[] args) throws Exception {
    Field field = BBB.class.getDeclaredField("type");
    FieldTest fieldTest = new FieldTest(field);
    System.out.println(fieldTest);
    BBB<Long> b=new BBB<Long>();
    fieldTest.field.set(b, BaseDataType.NATURAL.cast(157, fieldTest.type));
    System.out.println(b.type);
  }

  public static void testFluentIterable() {
    FluentIterable.of(1, 2, 3, 4, 5, 6, 7, 8, 9).transform(i -> {
      System.out.println(i);
      return i * 10;
    }).transform(i -> {
      System.out.println(i);
      return i + 1;
    }).toList();
  }

  public static void testTypeToken() {
    // Table<List<String>> table=new Table<List<String>>();
    // List<String> n=table.get();


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

    List<String> strings = Lists.newArrayList();
    TypeToken<List> typeToken = TypeToken.of(List.class);
    System.out.println(typeToken.isSubtypeOf(TypeToken.of(Collection.class)));
    System.out.println(typeToken.isSubtypeOf(new TypeToken<Collection>() {}));
    System.out.println(typeToken.isSubtypeOf(new TypeToken<Collection<String>>() {}));
    System.out.println(typeToken.isSubtypeOf(new TypeToken<Collection<Integer>>() {}));
  }

  public static void testCollector() {
    ImmutableList<Integer> list = Lists.newArrayList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).parallelStream()
        .peek(System.out::println).collect(ImmutableList.toImmutableList());
    System.out.println(list);
  }

  public static <T> void testTypeToken(TypeToken<T> token) {
    System.out.println(token.getRawType());
    TypeToken<?> genericToken = token.resolveType(token.getRawType().getTypeParameters()[0]);
    System.out.println(genericToken.getRawType());
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
