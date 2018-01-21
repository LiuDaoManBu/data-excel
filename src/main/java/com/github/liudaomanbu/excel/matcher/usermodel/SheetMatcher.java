package com.github.liudaomanbu.excel.matcher.usermodel;

import org.apache.poi.ss.usermodel.Sheet;
import com.github.liudaomanbu.excel.matcher.BaseMatcher;
import com.github.liudaomanbu.excel.matcher.constant.StringMatcherType;

public class SheetMatcher extends BaseMatcher<Sheet> {

  public SheetMatcher addNamePredicate(StringMatcherType type, String predicateValue) {
    add(type, predicateValue, Sheet::getSheetName);
    return this;
  }

}
