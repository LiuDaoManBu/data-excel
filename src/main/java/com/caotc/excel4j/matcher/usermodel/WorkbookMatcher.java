package com.caotc.excel4j.matcher.usermodel;

import java.util.List;
import java.util.function.Predicate;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.constant.Type;

public class WorkbookMatcher extends BaseMatcher<Workbook> {
  public static class Builder extends BaseMatcher.Builder<Workbook>{
    
    public WorkbookMatcher build(){
      return new WorkbookMatcher();
    }
    
    
  }
  public WorkbookMatcher(Type type, Matcher<Workbook> parent, List<Predicate<Workbook>> list) {
    super(type, parent, list);
  }

  public WorkbookMatcher(Type type, Matcher<Workbook> parent) {
    super(type, parent);
  }

  public WorkbookMatcher(Type type) {
    super(type);
  }

  public WorkbookMatcher() {
    super();
  }

  @Override
  public boolean test(Workbook t) {
    return super.test(t);
  }
  
}
