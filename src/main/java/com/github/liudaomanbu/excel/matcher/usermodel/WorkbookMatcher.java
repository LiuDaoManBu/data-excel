package com.github.liudaomanbu.excel.matcher.usermodel;

import org.apache.poi.ss.usermodel.Workbook;
import com.github.liudaomanbu.excel.matcher.BaseMatcher;
import com.github.liudaomanbu.excel.matcher.constant.ComparableMatcherType;

public class WorkbookMatcher extends BaseMatcher<Workbook> {
  public WorkbookMatcher addNumberOfSheetsPredicate(ComparableMatcherType type,
      int predicateValue) {
    add(type, predicateValue, Workbook::getNumberOfSheets);
    return this;
  }
}
