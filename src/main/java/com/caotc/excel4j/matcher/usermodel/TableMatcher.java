package com.caotc.excel4j.matcher.usermodel;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.StringMatcher.Builder.Expression;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.parse.result.Table;

public class TableMatcher extends BaseMatcher<Table> {
  public static class Builder extends BaseMatcher.Builder<Table> {
//    private List<Expression> nameExpressions;

    @Override
    public TableMatcher build() {
      return new TableMatcher(this);
    }

//    public List<Expression> getNameExpressions() {
//      return nameExpressions;
//    }
//
//    public Builder setNameExpressions(List<Expression> nameExpressions) {
//      this.nameExpressions = nameExpressions;
//      return this;
//    }
    
  }

  public static Builder builder() {
    return new Builder();
  }
  
  private TableMatcher(Builder builder) {
    super(builder);
//    if (Objects.nonNull(builder.nameExpressions)) {
//      builder.nameExpressions.stream().forEach(expression -> add(expression.getMatcherType(),
//          expression.getPredicateValue(), Sheet::getSheetName));
//    }
  }

//  public TableMatcher addNamePredicate(StringMatcherType type, String predicateValue) {
//    add(type, predicateValue, Sheet::getSheetName);
//    return this;
//  }
//
//  public TableMatcher addNameLengthPredicate(ComparableMatcherType type, int predicateValue) {
//    Function<Sheet, String> f = Sheet::getSheetName;
//    add(type, predicateValue, f.andThen(String::length));
//    return this;
//  }


}
