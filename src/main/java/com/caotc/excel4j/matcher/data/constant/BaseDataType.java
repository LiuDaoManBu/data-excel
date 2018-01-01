package com.caotc.excel4j.matcher.data.constant;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.util.TypeUtils;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.reflect.TypeToken;

public enum BaseDataType implements DataType {
  DECIMAL(float.class, Float.class, double.class, Double.class, BigDecimal.class) {
    @Override
    public boolean test(Object value) {
      try {
        return TypeUtils.castToBigDecimal(value).scale() > ZERO;
      } catch (NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  WHOLE_NUMBER(DECIMAL, byte.class, Byte.class, short.class, Short.class, int.class, Integer.class,
      long.class, Long.class, BigInteger.class) {
    @Override
    public boolean test(Object value) {
      try {
        TypeUtils.castToBigDecimal(value).toBigIntegerExact();
        return Boolean.TRUE;
      } catch (ArithmeticException | NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  NUMBER(DECIMAL.canCastTypes()) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) || WHOLE_NUMBER.test(value);
    }
  },
  POSITIVE_NUMBER(float.class, Float.class, double.class, Double.class, BigDecimal.class) {
    @Override
    public boolean test(Object value) {
      try {
        return NUMBER.test(value)
            && TypeUtils.castToBigDecimal(value).compareTo(BigDecimal.ZERO) > ZERO;
      } catch (NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  NEGATIVE_NUMBER(float.class, Float.class, double.class, Double.class, BigDecimal.class) {
    @Override
    public boolean test(Object value) {
      try {
        return NUMBER.test(value)
            && TypeUtils.castToBigDecimal(value).compareTo(BigDecimal.ZERO) < ZERO;
      } catch (NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  POSITIVE_WHOLE_NUMBER(
      FluentIterable.from(POSITIVE_NUMBER.types).filter(WHOLE_NUMBER.types::contains)) {
    @Override
    public boolean test(Object value) {
      return POSITIVE_NUMBER.test(value) && WHOLE_NUMBER.test(value);
    }
  },
  NEGATIVE_WHOLE_NUMBER(
      FluentIterable.from(NEGATIVE_NUMBER.types).filter(WHOLE_NUMBER.types::contains)) {
    @Override
    public boolean test(Object value) {
      return NEGATIVE_NUMBER.test(value) && WHOLE_NUMBER.test(value);
    }
  },
  NATURAL_NUMBER(POSITIVE_WHOLE_NUMBER.types) {
    @Override
    public boolean test(Object value) {
      try {
        return POSITIVE_WHOLE_NUMBER.test(value)
            || WHOLE_NUMBER.cast(value, BigInteger.class).equals(BigInteger.ZERO);
      } catch (ArithmeticException | NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  POSITIVE_DECIMAL(FluentIterable.from(POSITIVE_NUMBER.types).filter(DECIMAL.types::contains)) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) && POSITIVE_NUMBER.test(value);
    }
  },
  NEGATIVE_DECIMAL(FluentIterable.from(NEGATIVE_NUMBER.types).filter(DECIMAL.types::contains)) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) && NEGATIVE_NUMBER.test(value);
    }
  },
  DATE(Date.class, Calendar.class) {
    @Override
    public boolean test(Object value) {
      try {
        TypeUtils.castToDate(value);
        return Boolean.TRUE;
      } catch (JSONException e) {
        return Boolean.FALSE;
      }
    }
  };
  private static final int ZERO = 0;
  private final ImmutableCollection<TypeToken<?>> types;

  private BaseDataType(Iterable<TypeToken<?>> types) {
    this.types = ImmutableSet.copyOf(types);
  }

  private BaseDataType(Class<?>... types) {
    this(FluentIterable.from(types).transform(TypeToken::of));
  }

  private BaseDataType(DataType dataType, Class<?>... types) {
    FluentIterable<TypeToken<?>> typeTokens = FluentIterable.from(types).transform(TypeToken::of);
    typeTokens.append(dataType.canCastTypes());
    this.types = typeTokens.toSet();
  }

  @Override
  public ImmutableCollection<TypeToken<?>> canCastTypes() {
    return types;
  }

  @Override
  public <T> boolean canCast(TypeToken<T> type) {
    // TODO 父子类
    return types.contains(type);
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T cast(Object value, TypeToken<T> type) {
    // TODO
    if (FluentIterable.of(byte.class, Byte.class, short.class, Short.class, int.class,
        Integer.class, long.class, Long.class, BigInteger.class).transform(TypeToken::of)
        .contains(type)) {
      value = TypeUtils.castToBigDecimal(value).toBigIntegerExact();
    }
    return (T) TypeUtils.castToJavaBean(value, type.getRawType());
  }
}
