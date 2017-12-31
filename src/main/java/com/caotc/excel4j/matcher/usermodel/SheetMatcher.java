package com.caotc.excel4j.matcher.usermodel;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.constant.Type;

public class SheetMatcher extends BaseMatcher<Sheet> {

  public SheetMatcher(Type type, Matcher<Sheet> parent, List<Predicate<Sheet>> list) {
    super(type, parent, list);
  }

  public SheetMatcher(Type type, Matcher<Sheet> parent) {
    super(type, parent);
  }

  public SheetMatcher(Type type) {
    super(type);
  }

  public SheetMatcher() {
    super();
  }

  public SheetMatcher addNamePredicate(Predicate<String> predicate) {
    add(predicate, Sheet::getSheetName);
    return this;
  }

  public SheetMatcher addNamePredicate(StringMatcherType type, String predicateValue) {
    addNamePredicate(value -> type.apply(value, predicateValue));
    return this;
  }

  public SheetMatcher nameEquals(String predicateValue) {
    addNamePredicate(StringMatcherType.EQUALS, predicateValue);
    return this;
  }

  public SheetMatcher nameEqualsIgnoreCase(String predicateValue) {
    addNamePredicate(StringMatcherType.EQUALS_IGNORE_CASE, predicateValue);
    return this;
  }

  public SheetMatcher nameContains(String predicateValue) {
    addNamePredicate(StringMatcherType.CONTAINS, predicateValue);
    return this;
  }

  public SheetMatcher nameMatches(String predicateValue) {
    addNamePredicate(StringMatcherType.MATCHES, predicateValue);
    return this;
  }

  public SheetMatcher nameStartsWith(String predicateValue) {
    addNamePredicate(StringMatcherType.STARTS_WITH, predicateValue);
    return this;
  }

  public SheetMatcher nameEndsWith(String predicateValue) {
    addNamePredicate(StringMatcherType.ENDS_WITH, predicateValue);
    return this;
  }

  public SheetMatcher addNameLengthPredicate(Predicate<Integer> predicate) {
    Function<Sheet, String> f = Sheet::getSheetName;
    add(predicate, f.andThen(String::length));
    return this;
  }

  public SheetMatcher addNameLengthPredicate(ComparableMatcherType type, int predicateValue) {
    addNameLengthPredicate(value -> type.apply(value, predicateValue));
    return this;
  }

  public SheetMatcher nameLengthLt(int predicateValue) {
    addNameLengthPredicate(ComparableMatcherType.LT, predicateValue);
    return this;
  }

  public SheetMatcher nameLengthLe(int predicateValue) {
    addNameLengthPredicate(ComparableMatcherType.LE, predicateValue);
    return this;
  }

  public SheetMatcher nameLengthGt(int predicateValue) {
    addNameLengthPredicate(ComparableMatcherType.GT, predicateValue);
    return this;
  }

  public SheetMatcher nameLengthGe(int predicateValue) {
    addNameLengthPredicate(ComparableMatcherType.GE, predicateValue);
    return this;
  }

  public SheetMatcher nameLengthEq(int predicateValue) {
    addNameLengthPredicate(ComparableMatcherType.EQ, predicateValue);
    return this;
  }

  public SheetMatcher nameLengthNe(int predicateValue) {
    addNameLengthPredicate(ComparableMatcherType.NE, predicateValue);
    return this;
  }

  public SheetMatcher nameLengthBetween(int lowValue, int highValue) {
    addNameLengthPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
        && ComparableMatcherType.GE.apply(value, lowValue));
    return this;
  }
  
}
