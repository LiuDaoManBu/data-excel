package com.caotc.excel4j;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;
import com.caotc.excel4j.parse.result.Data;
import com.caotc.excel4j.util.ClassUtils;
import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;
import com.google.common.collect.TreeTraverser;
import com.google.common.reflect.TypeToken;
import com.google.common.reflect.TypeToken.TypeSet;

class AAA<T> {
  public void getType() {
    TypeToken typeToken = new TypeToken<AAA<T>>() {};
    TypeToken<?> genericTypeToken = typeToken.resolveType(AAA.class.getTypeParameters()[0]);
    System.out.println(genericTypeToken.getType());
  }

}


public class TestGuava {
  public static void main(String[] args) throws Exception{
    // whenFilterWithCollections2_thenFiltered();
    TypeToken<Number[]> stringTok = TypeToken.of(Number[].class);
    System.out.println(stringTok.isArray());
    System.out.println(stringTok.getType());
    TypeSet typeSet = stringTok.getTypes();
    System.out.println(typeSet);
    System.out.println(typeSet.classes());
    System.out.println(typeSet.interfaces());
    System.out.println(typeSet.classes().rawTypes());
    System.out.println(stringTok.getComponentType());
    System.out.println(stringTok.getRawType());

    System.out.println(ClassUtils.getAllSuperclasses(Object.class));
    TreeTraverser<String> tree =
        TreeTraverser.using(string -> string == null ? Lists.newArrayList("A", "B", "C")
            : Lists.newArrayList(string + "A", string + "B", string + "C"));
    System.out.println(tree.children(null));
    // System.out.println(tree.breadthFirstTraversal("A").toSet());
    System.out.println(ClassUtils.getAllFields(String.class));
    
    System.out.println();
    Field field=TypeToken.of(Data.class).getRawType().getDeclaredField("menuToCells");
    System.out.println(field.getType());
  }

  public static void viewTest2() {
    // Multimaps.filterEntries(unfiltered, entryPredicate)
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
