package com.caotc.excel4j.matcher.data.constant;

import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.util.TypeUtils;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.reflect.TypeToken;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.stream.Stream;

public enum BaseDataType implements DataType {
  DECIMAL(float.class, Float.class, double.class, Double.class, BigDecimal.class) {
    @Override
    public boolean test(Object value) {
      try {
        return TypeUtils.castToBigDecimal(value).scale() > 0;
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
  NUMBER(DECIMAL.canCastTypes().stream()) {
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
            && TypeUtils.castToBigDecimal(value).compareTo(BigDecimal.ZERO) > 0;
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
            && TypeUtils.castToBigDecimal(value).compareTo(BigDecimal.ZERO) < 0;
      } catch (NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  POSITIVE_WHOLE_NUMBER(POSITIVE_NUMBER.types.stream().filter(WHOLE_NUMBER.types::contains)) {
    @Override
    public boolean test(Object value) {
      return POSITIVE_NUMBER.test(value) && WHOLE_NUMBER.test(value);
    }
  },
  NEGATIVE_WHOLE_NUMBER(NEGATIVE_NUMBER.types.stream().filter(WHOLE_NUMBER.types::contains)) {
    @Override
    public boolean test(Object value) {
      return NEGATIVE_NUMBER.test(value) && WHOLE_NUMBER.test(value);
    }
  },
  NATURAL_NUMBER(POSITIVE_WHOLE_NUMBER.types.stream()) {
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
  POSITIVE_DECIMAL(POSITIVE_NUMBER.types.stream().filter(DECIMAL.types::contains)) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) && POSITIVE_NUMBER.test(value);
    }
  },
  NEGATIVE_DECIMAL(NEGATIVE_NUMBER.types.stream().filter(DECIMAL.types::contains)) {
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
  private static final ImmutableCollection<TypeToken<?>> INT_TYPES =
      Stream
          .of(byte.class, Byte.class, short.class, Short.class, int.class, Integer.class,
              long.class, Long.class, BigInteger.class)
          .map(TypeToken::of).collect(ImmutableSet.toImmutableSet());
  private final ImmutableCollection<TypeToken<?>> types;

  private BaseDataType(Stream<TypeToken<?>> types) {
    this.types = types.collect(ImmutableSet.toImmutableSet());
  }

  private BaseDataType(Class<?>... types) {
    this(Stream.of(types).map(TypeToken::of));
  }

  private BaseDataType(DataType dataType, Class<?>... types) {
    this.types =
        Stream.concat(Stream.of(types).map(TypeToken::of), dataType.canCastTypes().stream())
            .collect(ImmutableSet.toImmutableSet());
  }

  @Override
  public ImmutableCollection<TypeToken<?>> canCastTypes() {
    return types;
  }

  @Override
  public <T> boolean canCast(Class<T> type) {
    return canCast(TypeToken.of(type));
  }

  @Override
  public <T> boolean canCast(TypeToken<T> type) {
    // TODO 父子类
    return types.contains(type);
  }

  @Override
  public <T> T cast(Object value, Class<T> type) {
    return cast(value, TypeToken.of(type));
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T cast(Object value, TypeToken<T> type) {
    // TODO
    if (INT_TYPES.contains(type)) {
      value = TypeUtils.castToBigDecimal(value).toBigIntegerExact();
    }
    return (T) TypeUtils.castToJavaBean(value, type.getRawType());
  }
}
