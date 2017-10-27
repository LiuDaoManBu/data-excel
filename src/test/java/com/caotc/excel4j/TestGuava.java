package com.caotc.excel4j;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;
import com.google.common.reflect.TypeParameter;
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
    // whenFilterWithCollections2_thenFiltered();
//    TypeToken<Number[]> stringTok = TypeToken.of(Number[].class);
//    System.out.println(stringTok.isSupertypeOf(Integer.class));
//    System.out.println(stringTok.isArray());
//    System.out.println(stringTok.getType());
//    TypeSet typeSet = stringTok.getTypes();
//    System.out.println(typeSet);
//    System.out.println(typeSet.classes());
//    System.out.println(typeSet.interfaces());
//    System.out.println(typeSet.classes().rawTypes());
//    System.out.println(stringTok.getComponentType());
//    System.out.println(stringTok.getRawType());
//    System.out.println();
    Table<List<String>> table=new Table<List<String>>() {};
    List<String> n=table.get();
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
