package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;

public class StringMatcher extends BaseMatcher<String> {
  public static class Builder extends BaseMatcher.Builder<String> {
    public static class Expression {
      private StringMatcherType MatcherType;
      private String predicateValue;

      public StringMatcherType getMatcherType() {
        return MatcherType;
      }

      public void setMatcherType(StringMatcherType matcherType) {
        MatcherType = matcherType;
      }

      public String getPredicateValue() {
        return predicateValue;
      }

      public void setPredicateValue(String predicateValue) {
        this.predicateValue = predicateValue;
      }

    }

    private List<Expression> expressions;

    @Override
    public StringMatcher build() {
      //TODO 子类?
      if (Objects.nonNull(expressions)) {
        expressions.stream()
            .forEach(expression -> add(expression.MatcherType, expression.predicateValue));
      }
      return new StringMatcher(this);
    }

    public Builder add(StringMatcherType type, String predicateValue) {
      add(type, predicateValue, Function.identity());
      return this;
    }

    public Builder equals(String predicateValue) {
      add(StringMatcherType.EQUALS, predicateValue);
      return this;
    }

    public Builder equalsIgnoreCase(String predicateValue) {
      add(StringMatcherType.EQUALS_IGNORE_CASE, predicateValue);
      return this;
    }

    public Builder contains(String predicateValue) {
      add(StringMatcherType.CONTAINS, predicateValue);
      return this;
    }

    public Builder matches(String predicateValue) {
      add(StringMatcherType.MATCHES, predicateValue);
      return this;
    }

    public Builder startsWith(String predicateValue) {
      add(StringMatcherType.STARTS_WITH, predicateValue);
      return this;
    }

    public Builder endsWith(String predicateValue) {
      add(StringMatcherType.ENDS_WITH, predicateValue);
      return this;
    }

    public Builder addLengthPredicate(Predicate<Integer> predicate) {
      add(predicate, String::length);
      return this;
    }

    public Builder addLengthPredicate(ComparableMatcherType type, int predicateValue) {
      add(type, predicateValue, String::length);
      return this;
    }

    public Builder lengthLt(int predicateValue) {
      addLengthPredicate(ComparableMatcherType.LT, predicateValue);
      return this;
    }

    public Builder lengthLe(int predicateValue) {
      addLengthPredicate(ComparableMatcherType.LE, predicateValue);
      return this;
    }

    public Builder lengthGt(int predicateValue) {
      addLengthPredicate(ComparableMatcherType.GT, predicateValue);
      return this;
    }

    public Builder lengthGe(int predicateValue) {
      addLengthPredicate(ComparableMatcherType.GE, predicateValue);
      return this;
    }

    public Builder lengthEq(int predicateValue) {
      addLengthPredicate(ComparableMatcherType.EQ, predicateValue);
      return this;
    }

    public Builder lengthNe(int predicateValue) {
      addLengthPredicate(ComparableMatcherType.NE, predicateValue);
      return this;
    }

    public Builder lengthBetween(int lowValue, int highValue) {
      addLengthPredicate(value -> ComparableMatcherType.LE.apply(value, highValue)
          && ComparableMatcherType.GE.apply(value, lowValue));
      return this;
    }

    public List<Expression> getExpressions() {
      return expressions;
    }

    public Builder setExpressions(List<Expression> expressions) {
      this.expressions = expressions;
      return this;
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  private StringMatcher(Builder builder) {
    super(builder);
  }

}
