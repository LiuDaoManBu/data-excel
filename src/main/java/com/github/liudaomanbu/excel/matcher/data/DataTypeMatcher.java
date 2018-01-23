package com.github.liudaomanbu.excel.matcher.data;

import java.util.function.Predicate;
import com.github.liudaomanbu.excel.matcher.BaseMatcher;
import com.github.liudaomanbu.excel.matcher.constant.ComparableMatcherType;
import com.github.liudaomanbu.excel.matcher.constant.StringMatcherType;
import com.github.liudaomanbu.excel.matcher.data.type.DataType;
import com.google.common.reflect.TypeToken;

public class DataTypeMatcher extends BaseMatcher<Object> implements DataMatcher {
  private DataType dataType;

  public <T> DataTypeMatcher addDataPredicate(Predicate<T> predicate, TypeToken<T> type) {
    add(predicate, value -> dataType.cast(value, type));
    return this;
  }

  public DataTypeMatcher addDataPredicate(StringMatcherType type, String predicateValue) {
    add(type, predicateValue, value -> dataType.cast(value, String.class));
    return this;
  }

  public <T extends Comparable<T>> DataTypeMatcher addDataPredicate(ComparableMatcherType type,
      T predicateValue) {
    add(type, predicateValue, value ->(T)dataType.cast(value, predicateValue.getClass()));
    return this;
  }

  public DataType getDataType() {
    return dataType;
  }

  public DataTypeMatcher setDataType(DataType dataType) {
    this.dataType = dataType;
    return this;
  }

  // @SuppressWarnings({"rawtypes", "unchecked"})
  // private static <T> TypeToken<T> findType(Predicate<T> predicate) {
  // TypeToken<? extends Predicate> token = TypeToken.of(predicate.getClass());
  // TypeVariable<?>[] types = token.getSupertype(Predicate.class).getRawType().getTypeParameters();
  // return TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
  // }
}
