package com.caotc.excel4j;

import java.util.Collection;
import java.util.List;
import javax.annotation.Nullable;
import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;
import com.google.common.reflect.TypeToken;

class Table<T> {
  public T get() {
    TypeToken<T> typeToken = new TypeToken<T>(getClass()) {};
    System.out.println(typeToken);
    System.out.println(typeToken.getRawType());
    TypeToken<?> token=typeToken.resolveType(typeToken.getRawType().getTypeParameters()[0]);
    System.out.println(token);
    return null;
  }
}

public class TestGuava {
  public static void main(String[] args) throws Exception{
//    Table<List<String>> table=new Table<List<String>>() {};
//    List<String> n=table.get();
    
//    System.out.println(int[][][].class.equals(int[].class));
    
    Class<?> type=void.class;
    System.out.println(type.isInterface());
    System.out.println(type.isLocalClass());
    System.out.println(type.isMemberClass());
    System.out.println(type.isSynthetic());
    
    TypeToken token=TypeToken.of(void.class);
    System.out.println(token.isPrimitive());
  }

  public static void whenFilterWithCollections2_thenFiltered() {
    List<String> names = Lists.newArrayList("John", "Jane", "Adam", "Tom");
    Collection<String> result = Collections2.filter(names, Predicates.containsPattern("a"));
    System.out.println(Collections2.transform(names, string -> string.length()));
    System.out.println(names);
    System.out.println(result);

    System.out.println();
    result.add("anna");
    // names.add("anna");
    System.out.println(names);
    System.out.println(result);
  }
}
