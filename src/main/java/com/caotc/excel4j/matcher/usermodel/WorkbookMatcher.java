package com.caotc.excel4j.matcher.usermodel;

import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;

public class WorkbookMatcher extends BaseMatcher<Workbook> {
  public WorkbookMatcher addNumberOfSheetsPredicate(ComparableMatcherType type,
      int predicateValue) {
    add(type, predicateValue, Workbook::getNumberOfSheets);
    return this;
  }
}
