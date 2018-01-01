package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.constant.Type;

public class StringMatcher extends BaseMatcher<String> {

  public StringMatcher(Type type, Matcher<String> parent, List<Predicate<String>> list) {
    super(type, parent, list);
  }

  public StringMatcher(Type type, Matcher<String> parent) {
    super(type, parent);
  }

  public StringMatcher(Type type) {
    super(type);
  }

  public StringMatcher() {
    super();
  }

  public StringMatcher add(StringMatcherType type, String predicateValue) {
    add(type, predicateValue, Function.identity());
    return this;
  }

  public StringMatcher equals(String predicateValue) {
    add(StringMatcherType.EQUALS, predicateValue);
    return this;
  }

  public StringMatcher equalsIgnoreCase(String predicateValue) {
    add(StringMatcherType.EQUALS_IGNORE_CASE, predicateValue);
    return this;
  }

  public StringMatcher contains(String predicateValue) {
    add(StringMatcherType.CONTAINS, predicateValue);
    return this;
  }

  public StringMatcher matches(String predicateValue) {
    add(StringMatcherType.MATCHES, predicateValue);
    return this;
  }

  public StringMatcher startsWith(String predicateValue) {
    add(StringMatcherType.STARTS_WITH, predicateValue);
    return this;
  }

  public StringMatcher endsWith(String predicateValue) {
    add(StringMatcherType.ENDS_WITH, predicateValue);
    return this;
  }

  public StringMatcher addLengthPredicate(Predicate<Integer> predicate) {
    add(predicate, String::length);
    return this;
  }

  public StringMatcher addLengthPredicate(ComparableMatcherType type, int predicateValue) {
    add(type,predicateValue,String::length);
    return this;
  }

  public StringMatcher lengthLt(int predicateValue) {
    addLengthPredicate(ComparableMatcherType.LT, predicateValue);
    return this;
  }

  public StringMatcher lengthLe(int predicateValue) {
    addLengthPredicate(ComparableMatcherType.LE, predicateValue);
    return this;
  }

  public StringMatcher lengthGt(int predicateValue) {
    addLengthPredicate(ComparableMatcherType.GT, predicateValue);
    return this;
  }

  public StringMatcher lengthGe(int predicateValue) {
    addLengthPredicate(ComparableMatcherType.GE, predicateValue);
    return this;
  }

  public StringMatcher lengthEq(int predicateValue) {
    addLengthPredicate(ComparableMatcherType.EQ, predicateValue);
    return this;
  }

  public StringMatcher lengthNe(int predicateValue) {
    addLengthPredicate(ComparableMatcherType.NE, predicateValue);
    return this;
  }

  public StringMatcher lengthBetween(int lowValue, int highValue) {
    addLengthPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
        && ComparableMatcherType.GE.apply(value, lowValue));
    return this;
  }
}
