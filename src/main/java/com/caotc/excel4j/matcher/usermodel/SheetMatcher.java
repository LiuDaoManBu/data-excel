package com.caotc.excel4j.matcher.usermodel;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.StringMatcher.Builder.Expression;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.constant.Type;

public class SheetMatcher extends BaseMatcher<Sheet> {
  public static class Builder extends BaseMatcher.Builder<Sheet> {
    private List<Expression> nameExpressions;

    @Override
    public SheetMatcher build() {
      return new SheetMatcher(this);
    }

    public List<Expression> getNameExpressions() {
      return nameExpressions;
    }

    public Builder setNameExpressions(List<Expression> nameExpressions) {
      this.nameExpressions = nameExpressions;
      return this;
    }
    
  }

  private SheetMatcher(Builder builder) {
    super(builder);
    if (Objects.nonNull(builder.nameExpressions)) {
      builder.nameExpressions.stream().forEach(expression -> add(expression.getMatcherType(),
          expression.getPredicateValue(), Sheet::getSheetName));
    }
  }
  
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

  public SheetMatcher addNamePredicate(StringMatcherType type, String predicateValue) {
    add(type, predicateValue, Sheet::getSheetName);
    return this;
  }

  public SheetMatcher addNameLengthPredicate(ComparableMatcherType type, int predicateValue) {
    Function<Sheet, String> f = Sheet::getSheetName;
    add(type, predicateValue, f.andThen(String::length));
    return this;
  }


}
