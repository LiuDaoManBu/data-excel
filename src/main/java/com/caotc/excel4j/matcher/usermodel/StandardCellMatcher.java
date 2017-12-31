package com.caotc.excel4j.matcher.usermodel;

import java.util.function.Function;
import java.util.function.Predicate;
import org.apache.poi.hssf.util.CellReference;
import org.apache.poi.ss.usermodel.CellType;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.parse.result.StandardCell;

public class StandardCellMatcher extends BaseMatcher<StandardCell> {
  public StandardCellMatcher addCellTypePredicate(Predicate<CellType> predicate) {
    add(predicate, StandardCell::getCellTypeEnum);
    return this;
  }

  public StandardCellMatcher addFirstRowIndexPredicate(Predicate<Integer> predicate) {
    add(predicate, StandardCell::getFirstRow);
    return this;
  }

  public StandardCellMatcher addFirstRowIndexPredicate(ComparableMatcherType type, int predicateValue) {
    addFirstRowIndexPredicate(value -> type.apply(value, predicateValue));
    return this;
  }
  
  public StandardCellMatcher firstRowIndexLt(int predicateValue) {
    addFirstRowIndexPredicate(ComparableMatcherType.LT, predicateValue);
    return this;
  }

  public StandardCellMatcher firstRowIndexLe(int predicateValue) {
    addFirstRowIndexPredicate(ComparableMatcherType.LE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstRowIndexGt(int predicateValue) {
    addFirstRowIndexPredicate(ComparableMatcherType.GT, predicateValue);
    return this;
  }

  public StandardCellMatcher firstRowIndexGe(int predicateValue) {
    addFirstRowIndexPredicate(ComparableMatcherType.GE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstRowIndexEq(int predicateValue) {
    addFirstRowIndexPredicate(ComparableMatcherType.EQ, predicateValue);
    return this;
  }

  public StandardCellMatcher firstRowIndexNe(int predicateValue) {
    addFirstRowIndexPredicate(ComparableMatcherType.NE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstRowIndexBetween(int lowValue, int highValue) {
    addFirstRowIndexPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
        && ComparableMatcherType.GE.apply(value, lowValue));
    return this;
  }
  
  public StandardCellMatcher addLastRowIndexPredicate(Predicate<Integer> predicate) {
    add(predicate, StandardCell::getLastRow);
    return this;
  }

  public StandardCellMatcher addLastRowIndexPredicate(ComparableMatcherType type, int predicateValue) {
    addLastRowIndexPredicate(value -> type.apply(value, predicateValue));
    return this;
  }
  
  public StandardCellMatcher lastRowIndexLt(int predicateValue) {
    addLastRowIndexPredicate(ComparableMatcherType.LT, predicateValue);
    return this;
  }

  public StandardCellMatcher lastRowIndexLe(int predicateValue) {
    addLastRowIndexPredicate(ComparableMatcherType.LE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastRowIndexGt(int predicateValue) {
    addLastRowIndexPredicate(ComparableMatcherType.GT, predicateValue);
    return this;
  }

  public StandardCellMatcher lastRowIndexGe(int predicateValue) {
    addLastRowIndexPredicate(ComparableMatcherType.GE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastRowIndexEq(int predicateValue) {
    addLastRowIndexPredicate(ComparableMatcherType.EQ, predicateValue);
    return this;
  }

  public StandardCellMatcher lastRowIndexNe(int predicateValue) {
    addLastRowIndexPredicate(ComparableMatcherType.NE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastRowIndexBetween(int lowValue, int highValue) {
    addLastRowIndexPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
        && ComparableMatcherType.GE.apply(value, lowValue));
    return this;
  }
  
  public StandardCellMatcher addFirstColumnIndexPredicate(Predicate<Integer> predicate) {
    add(predicate, StandardCell::getFirstColumn);
    return this;
  }

  public StandardCellMatcher addFirstColumnIndexPredicate(ComparableMatcherType type, int predicateValue) {
    addFirstColumnIndexPredicate(value -> type.apply(value, predicateValue));
    return this;
  }
  
  public StandardCellMatcher firstColumnIndexLt(int predicateValue) {
    addFirstColumnIndexPredicate(ComparableMatcherType.LT, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexLe(int predicateValue) {
    addFirstColumnIndexPredicate(ComparableMatcherType.LE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexGt(int predicateValue) {
    addFirstColumnIndexPredicate(ComparableMatcherType.GT, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexGe(int predicateValue) {
    addFirstColumnIndexPredicate(ComparableMatcherType.GE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexEq(int predicateValue) {
    addFirstColumnIndexPredicate(ComparableMatcherType.EQ, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexNe(int predicateValue) {
    addFirstColumnIndexPredicate(ComparableMatcherType.NE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexBetween(int lowValue, int highValue) {
    addFirstColumnIndexPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
        && ComparableMatcherType.GE.apply(value, lowValue));
    return this;
  }
  
  public StandardCellMatcher addLastColumnIndexPredicate(Predicate<Integer> predicate) {
    add(predicate, StandardCell::getLastColumn);
    return this;
  }

  public StandardCellMatcher addLastColumnIndexPredicate(ComparableMatcherType type, int predicateValue) {
    addLastColumnIndexPredicate(value -> type.apply(value, predicateValue));
    return this;
  }
  
  public StandardCellMatcher lastColumnIndexLt(int predicateValue) {
    addLastColumnIndexPredicate(ComparableMatcherType.LT, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexLe(int predicateValue) {
    addLastColumnIndexPredicate(ComparableMatcherType.LE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexGt(int predicateValue) {
    addLastColumnIndexPredicate(ComparableMatcherType.GT, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexGe(int predicateValue) {
    addLastColumnIndexPredicate(ComparableMatcherType.GE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexEq(int predicateValue) {
    addLastColumnIndexPredicate(ComparableMatcherType.EQ, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexNe(int predicateValue) {
    addLastColumnIndexPredicate(ComparableMatcherType.NE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexBetween(int lowValue, int highValue) {
    addLastColumnIndexPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
        && ComparableMatcherType.GE.apply(value, lowValue));
    return this;
  }
  
  public StandardCellMatcher addFirstColumnIndexStringPredicate(Predicate<String> predicate) {
    Function<StandardCell, Integer> f = StandardCell::getFirstColumn;
    add(predicate, f.andThen(CellReference::convertNumToColString));
    return this;
  }

  public StandardCellMatcher addFirstColumnIndexStringPredicate(StringMatcherType type, String predicateValue) {
    addFirstColumnIndexStringPredicate(value -> type.apply(value, predicateValue));
    return this;
  }

  public StandardCellMatcher firstColumnIndexStringEquals(String predicateValue) {
    addFirstColumnIndexStringPredicate(StringMatcherType.EQUALS, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexStringEqualsIgnoreCase(String predicateValue) {
    addFirstColumnIndexStringPredicate(StringMatcherType.EQUALS_IGNORE_CASE, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexStringContains(String predicateValue) {
    addFirstColumnIndexStringPredicate(StringMatcherType.CONTAINS, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexStringMatches(String predicateValue) {
    addFirstColumnIndexStringPredicate(StringMatcherType.MATCHES, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexStringStartsWith(String predicateValue) {
    addFirstColumnIndexStringPredicate(StringMatcherType.STARTS_WITH, predicateValue);
    return this;
  }

  public StandardCellMatcher firstColumnIndexStringEndsWith(String predicateValue) {
    addFirstColumnIndexStringPredicate(StringMatcherType.ENDS_WITH, predicateValue);
    return this;
  }
  
  public StandardCellMatcher addLastColumnIndexStringPredicate(Predicate<String> predicate) {
    Function<StandardCell, Integer> f = StandardCell::getLastColumn;
    add(predicate, f.andThen(CellReference::convertNumToColString));
    return this;
  }
  
  public StandardCellMatcher addLastColumnIndexStringPredicate(StringMatcherType type, String predicateValue) {
    addLastColumnIndexStringPredicate(value -> type.apply(value, predicateValue));
    return this;
  }

  public StandardCellMatcher lastColumnIndexStringEquals(String predicateValue) {
    addLastColumnIndexStringPredicate(StringMatcherType.EQUALS, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexStringEqualsIgnoreCase(String predicateValue) {
    addLastColumnIndexStringPredicate(StringMatcherType.EQUALS_IGNORE_CASE, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexStringContains(String predicateValue) {
    addLastColumnIndexStringPredicate(StringMatcherType.CONTAINS, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexStringMatches(String predicateValue) {
    addLastColumnIndexStringPredicate(StringMatcherType.MATCHES, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexStringStartsWith(String predicateValue) {
    addLastColumnIndexStringPredicate(StringMatcherType.STARTS_WITH, predicateValue);
    return this;
  }

  public StandardCellMatcher lastColumnIndexStringEndsWith(String predicateValue) {
    addLastColumnIndexStringPredicate(StringMatcherType.ENDS_WITH, predicateValue);
    return this;
  }
}
