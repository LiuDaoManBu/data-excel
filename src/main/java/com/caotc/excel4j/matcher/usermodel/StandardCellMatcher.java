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
  public StandardCellMatcher addDataPredicate(Predicate<Object> predicate) {
    add(predicate, StandardCell::getValue);
    return this;
  }

  public <T> StandardCellMatcher addDataPredicate(Predicate<T> predicate,
      Function<Object, T> transformer) {
    add(predicate, transformer.compose(StandardCell::getValue));
    return this;
  }

  public StandardCellMatcher addDataPredicate(StringMatcherType type, String predicateValue,
      Function<Object, String> transformer) {
    add(type, predicateValue, transformer.compose(StandardCell::getValue));
    return this;
  }

  public <T extends Comparable<T>> StandardCellMatcher addDataPredicate(ComparableMatcherType type,
      T predicateValue, Function<Object, T> transformer) {
    add(type, predicateValue, transformer.compose(StandardCell::getValue));
    return this;
  }

  public StandardCellMatcher addCellTypePredicate(Predicate<CellType> predicate) {
    add(predicate, StandardCell::getCellTypeEnum);
    return this;
  }

  public StandardCellMatcher addFirstRowIndexPredicate(ComparableMatcherType type,
      int predicateValue) {
    add(type, predicateValue, StandardCell::getFirstRow);
    return this;
  }

  public StandardCellMatcher addLastRowIndexPredicate(ComparableMatcherType type,
      int predicateValue) {
    add(type, predicateValue, StandardCell::getLastRow);
    return this;
  }

  public StandardCellMatcher addFirstColumnIndexPredicate(ComparableMatcherType type,
      int predicateValue) {
    add(type, predicateValue, StandardCell::getFirstColumn);
    return this;
  }

  public StandardCellMatcher addLastColumnIndexPredicate(ComparableMatcherType type,
      int predicateValue) {
    add(type, predicateValue, StandardCell::getLastColumn);
    return this;
  }

  public StandardCellMatcher addFirstColumnIndexStringPredicate(StringMatcherType type,
      String predicateValue) {
    Function<StandardCell, Integer> f = StandardCell::getFirstColumn;
    add(type, predicateValue, f.andThen(CellReference::convertNumToColString));
    return this;
  }

  public StandardCellMatcher addLastColumnIndexStringPredicate(StringMatcherType type,
      String predicateValue) {
    Function<StandardCell, Integer> f = StandardCell::getLastColumn;
    add(type, predicateValue, f.andThen(CellReference::convertNumToColString));
    return this;
  }
}
