package com.caotc.excel4j.matcher.usermodel;

import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.constant.StringMatcherType;

public class SheetMatcher extends BaseMatcher<Sheet> {

  public SheetMatcher addNamePredicate(StringMatcherType type, String predicateValue) {
    add(type, predicateValue, Sheet::getSheetName);
    return this;
  }

}
