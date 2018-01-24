package com.github.liudaomanbu.excel;

import java.util.Objects;
import java.util.function.Predicate;

public class TestMatcher {
  public static void main(String[] args) {
    testLambda();
  }
  
  public static void testLambda() {
    Predicate<String> a=Objects::isNull;
    Predicate<String> b=Objects::isNull;
    System.out.println(a.equals(b));
  }
}
