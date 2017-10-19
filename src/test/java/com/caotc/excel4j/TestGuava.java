package com.caotc.excel4j;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import com.google.common.base.Predicates;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class TestGuava {
  public static void main(String[] args) {
//    whenFilterWithCollections2_thenFiltered();
    ImmutableCollection<String> strings=ImmutableList.of("aaa","b");
    Collection<String> filters=Collections2.filter(strings, String::isEmpty);
    ImmutableCollection<String> list=(ImmutableCollection<String>) filters;
  }

  public static void viewTest() {
    TestGuavaA a1 = new TestGuavaA(1L, "A", "caotc");
    TestGuavaA a2 = new TestGuavaA(2L, "A", "caotc");
    TestGuavaA a3 = new TestGuavaA(3L, "A", "abel");
    TestGuavaA b1 = new TestGuavaA(4L, "B", "abel");
    TestGuavaA b2 = new TestGuavaA(5L, "B", "caotc");

    Set<TestGuavaA> list = Sets.newHashSet(a1, a2, a3, b1, b2);
    System.out.println(list);
//    System.out.println(map);

    System.out.println();
    // list.add(new TestGuavaA(6L, "B", "xies"));
//    map.put(6L, new TestGuavaA(6L, "B", "xies"));
    System.out.println(list);
//    System.out.println(map);
  }

  public static void viewTest2() {
    // Multimaps.filterEntries(unfiltered, entryPredicate)
  }

  public static void whenFilterWithCollections2_thenFiltered() {
    List<String> names = Lists.newArrayList("John", "Jane", "Adam", "Tom");
    Collection<String> result = Collections2.filter(names, Predicates.containsPattern("a"));
    System.out.println(Collections2.transform(names,string->string.length()));
    System.out.println(names);
    System.out.println(result);

    System.out.println();
     result.add("anna");
//    names.add("anna");
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
