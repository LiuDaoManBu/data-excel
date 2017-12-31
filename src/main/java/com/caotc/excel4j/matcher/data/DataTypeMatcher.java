package com.caotc.excel4j.matcher.data;

import java.lang.reflect.TypeVariable;
import java.util.List;
import java.util.function.Predicate;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.constant.Type;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.reflect.TypeToken;

public abstract class DataTypeMatcher<T> extends BaseMatcher<Object> implements DataMatcher {
  private final DataType dataType;
  //TODO 改为动态传入?
  private final TypeToken<T> type;

  public DataTypeMatcher(DataType dataType) {
    super();
    this.dataType = dataType;
    type = findType();
  }

  public DataTypeMatcher(Type type, DataType dataType) {
    super(type);
    this.dataType = dataType;
    this.type = findType();
  }

  public DataTypeMatcher(Type type, Matcher<Object> parent, DataType dataType) {
    super(type, parent);
    this.dataType = dataType;
    this.type = findType();
  }

  public DataTypeMatcher(Type type, Matcher<Object> parent, List<Predicate<Object>> list,
      DataType dataType) {
    super(type, parent, list);
    this.dataType = dataType;
    this.type = findType();
  }

  @Override
  public boolean support(Object value) {
    return dataType.test(value);
  }

  public DataMatcher addDataPredicate(Predicate<T> predicate) {
    add(predicate,this::cast);
    return this;
  }

  private T cast(Object value) {
    return dataType.cast(value, type);
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
  private TypeToken<T> findType() {
    TypeToken<? extends DataTypeMatcher> token = TypeToken.of(getClass());
    TypeVariable<?>[] types =
        token.getSupertype(DataTypeMatcher.class).getRawType().getTypeParameters();
    return TypeToken.of((Class<T>) token.resolveType(types[0]).getRawType());
  }

  public TypeToken<T> getType() {
    return type;
  }
}
