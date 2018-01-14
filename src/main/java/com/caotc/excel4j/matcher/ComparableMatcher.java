package com.caotc.excel4j.matcher;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;

public class ComparableMatcher<T extends Comparable<T>> extends BaseMatcher<T> {
  public static class Builder<T extends Comparable<T>> extends BaseMatcher.Builder<T> {
    public static class Expression<T extends Comparable<T>> {
      private ComparableMatcherType MatcherType;
      private T predicateValue;

      public ComparableMatcherType getMatcherType() {
        return MatcherType;
      }

      public void setMatcherType(ComparableMatcherType matcherType) {
        MatcherType = matcherType;
      }

      public T getPredicateValue() {
        return predicateValue;
      }

      public void setPredicateValue(T predicateValue) {
        this.predicateValue = predicateValue;
      }
    }

    private List<Expression<T>> expressions;

    @Override
    public ComparableMatcher<T> build() {
      return new ComparableMatcher<T>(this);
    }

    public Builder<T> add(ComparableMatcherType type, T predicateValue) {
      add(type, predicateValue, Function.identity());
      return this;
    }

    public Builder<T> lt(T predicateValue) {
      add(ComparableMatcherType.LT, predicateValue);
      return this;
    }

    public Builder<T> le(T predicateValue) {
      add(ComparableMatcherType.LE, predicateValue);
      return this;
    }

    public Builder<T> gt(T predicateValue) {
      add(ComparableMatcherType.GT, predicateValue);
      return this;
    }

    public Builder<T> ge(T predicateValue) {
      add(ComparableMatcherType.GE, predicateValue);
      return this;
    }

    public Builder<T> eq(T predicateValue) {
      add(ComparableMatcherType.EQ, predicateValue);
      return this;
    }

    public Builder<T> ne(T predicateValue) {
      add(ComparableMatcherType.NE, predicateValue);
      return this;
    }

    // TODO between和其他type对于参数个数不同要求的处理?
    public Builder<T> between(T lowValue, T highValue) {
      add(value -> ComparableMatcherType.LE.apply(value, highValue)
          && ComparableMatcherType.GE.apply(value, lowValue));
      return this;
    }
    
    public List<Expression<T>> getExpressions() {
      return expressions;
    }

    public Builder<T> setExpressions(List<Expression<T>> expressions) {
      this.expressions = expressions;
      return this;
    }

  }

  public static <T extends Comparable<T>> Builder<T> builder() {
    return new Builder<>();
  }

  private ComparableMatcher(Builder<T> builder) {
    super(builder);
  //TODO
//    if (Objects.nonNull(builder.expressions)) {
//      builder.expressions.stream()
//          .forEach(expression -> add(expression.MatcherType, expression.predicateValue));
//    }
  }

}
