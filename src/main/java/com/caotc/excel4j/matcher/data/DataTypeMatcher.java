package com.caotc.excel4j.matcher.data;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.base.Preconditions;
import com.google.common.reflect.TypeToken;

public class DataTypeMatcher extends BaseMatcher<Object> implements DataMatcher {
  public static class Builder extends BaseMatcher.Builder<Object> {
    // public static class Expression{
    //
    // }
    private BaseDataType baseDataType;
    private DataType dataType;
    // TODO json时String转Class?
    // private List<Expression> nameExpressions;

    @Override
    public DataTypeMatcher build() {
      dataType = Optional.ofNullable(dataType).orElse(baseDataType);
      // TODO 提示语
      Preconditions.checkState(Objects.nonNull(dataType));
      return new DataTypeMatcher(this);
    }

    public <T> Builder addDataPredicate(Predicate<T> predicate, TypeToken<T> type) {
      add(predicate, value -> dataType.cast(value, type));
      return this;
    }

    public Builder addDataPredicate(StringMatcherType type, String predicateValue) {
      add(type, predicateValue, value -> dataType.cast(value, String.class));
      return this;
    }

    public <T extends Comparable<T>> Builder addDataPredicate(ComparableMatcherType type,
        T predicateValue) {
      // TODO safe?
      add(type, predicateValue, value -> (T) dataType.cast(value, predicateValue.getClass()));
      return this;
    }

    public BaseDataType getBaseDataType() {
      return baseDataType;
    }

    public Builder setBaseDataType(BaseDataType baseDataType) {
      this.baseDataType = baseDataType;
      return this;
    }

    public DataType getDataType() {
      return dataType;
    }

    public Builder setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    // public List<Expression> getNameExpressions() {
    // return nameExpressions;
    // }
    //
    // public Builder setNameExpressions(List<Expression> nameExpressions) {
    // this.nameExpressions = nameExpressions;
    // return this;
    // }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final DataType dataType;

  private DataTypeMatcher(Builder builder) {
    super(builder.add(builder.dataType::test));
    this.dataType = builder.dataType;
    // if (Objects.nonNull(builder.nameExpressions)) {
    // builder.nameExpressions.stream().forEach(expression -> add(expression.getMatcherType(),
    // expression.getPredicateValue(), standardCell->dataType.cast(standardCell.getValue(),
    // String.class)));
    // }
  }

  @Override
  // TODO need?
  public boolean support(Object value) {
    return dataType.test(value);
  }

  // @SuppressWarnings({"rawtypes", "unchecked"})
  // private static <T> TypeToken<T> findType(Predicate<T> predicate) {
  // TypeToken<? extends Predicate> token = TypeToken.of(predicate.getClass());
  // TypeVariable<?>[] types = token.getSupertype(Predicate.class).getRawType().getTypeParameters();
  // return TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
  // }
}
