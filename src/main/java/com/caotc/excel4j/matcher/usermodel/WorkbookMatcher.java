package com.caotc.excel4j.matcher.usermodel;

import java.util.List;
import java.util.Objects;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.ComparableMatcher.Builder.Expression;

public class WorkbookMatcher extends BaseMatcher<Workbook> {
  public static class Builder extends BaseMatcher.Builder<Workbook> {
    private List<Expression<Integer>> sheetSizeExpressions;

    @Override
    public WorkbookMatcher build() {
      return new WorkbookMatcher(this);
    }

    public List<Expression<Integer>> getSheetSizeExpressions() {
      return sheetSizeExpressions;
    }

    public Builder setSheetSizeExpressions(List<Expression<Integer>> sheetSizeExpressions) {
      this.sheetSizeExpressions = sheetSizeExpressions;
      return this;
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  private WorkbookMatcher(Builder builder) {
    super(builder);
    //TODO
//    if (Objects.nonNull(builder.sheetSizeExpressions)) {
//      builder.sheetSizeExpressions.stream().forEach(expression -> add(expression.getMatcherType(),
//          expression.getPredicateValue(), Workbook::getNumberOfSheets));
//    }
  }
}
