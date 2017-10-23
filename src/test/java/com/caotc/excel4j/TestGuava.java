package com.caotc.excel4j;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import com.caotc.excel4j.util.ClassUtils;
import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.collect.TreeTraverser;
import com.google.common.reflect.TypeToken;
import com.google.common.reflect.TypeToken.TypeSet;

public class TestGuava {
  public static <T> void main(String[] args) {
    // whenFilterWithCollections2_thenFiltered();
    TypeToken<Number> stringTok = TypeToken.of(Number.class);
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
//    System.out.println(tree.breadthFirstTraversal("A").toSet());
     System.out.println(ClassUtils.getAllFields(String.class));
     System.out.println(Iterables.isEmpty(null));
  }
  
  public static void viewTest() {
    TestGuavaA a1 = new TestGuavaA(1L, "A", "caotc");
    TestGuavaA a2 = new TestGuavaA(2L, "A", "caotc");
    TestGuavaA a3 = new TestGuavaA(3L, "A", "abel");
    TestGuavaA b1 = new TestGuavaA(4L, "B", "abel");
    TestGuavaA b2 = new TestGuavaA(5L, "B", "caotc");

    Set<TestGuavaA> list = Sets.newHashSet(a1, a2, a3, b1, b2);
    System.out.println(list);
    // System.out.println(map);

    System.out.println();
    // list.add(new TestGuavaA(6L, "B", "xies"));
    // map.put(6L, new TestGuavaA(6L, "B", "xies"));
    System.out.println(list);
    // System.out.println(map);
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

  static class TestGuavaA {
    public TestGuavaA(Long id, String type, String author) {
      super();
      this.id = id;
      this.type = type;
      this.author = author;
    }

    private Long id;
    private String type;
    private String author;

    @Override
    public String toString() {
      return ToStringBuilder.reflectionToString(this, ToStringStyle.NO_CLASS_NAME_STYLE);
    }
  }
}
