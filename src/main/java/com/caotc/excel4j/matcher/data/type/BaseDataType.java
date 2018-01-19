package com.caotc.excel4j.matcher.data.type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.stream.Stream;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.util.TypeUtils;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.reflect.TypeToken;

public enum BaseDataType implements DataType {
  // 小数
  DECIMAL(float.class, Float.class, double.class, Double.class, BigDecimal.class, String.class) {
    @Override
    public boolean test(Object value) {
      try {
        return TypeUtils.castToBigDecimal(value).scale() > 0;
      } catch (NumberFormatException e) {
        return Boolean.FALSE;
      }
    }
  },
  // 整数
  WHOLE_NUMBER(Stream
      .concat(DECIMAL.types.stream(),
          Stream.of(byte.class, Byte.class, short.class, Short.class, int.class, Integer.class,
              long.class, Long.class, BigInteger.class).map(TypeToken::of))
      .collect(ImmutableSet.toImmutableSet())) {
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
  // 数字
  NUMBER(DECIMAL.types) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) || WHOLE_NUMBER.test(value);
    }
  },
  // 正数
  POSITIVE_NUMBER(NUMBER.types) {
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
  // 负数
  NEGATIVE_NUMBER(NUMBER.types) {
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
  // 正整数
  POSITIVE_WHOLE_NUMBER(POSITIVE_NUMBER.types.stream().filter(WHOLE_NUMBER.types::contains)
      .collect(ImmutableSet.toImmutableSet())) {
    @Override
    public boolean test(Object value) {
      return POSITIVE_NUMBER.test(value) && WHOLE_NUMBER.test(value);
    }
  },
  // 负整数
  NEGATIVE_WHOLE_NUMBER(NEGATIVE_NUMBER.types.stream().filter(WHOLE_NUMBER.types::contains)
      .collect(ImmutableSet.toImmutableSet())) {
    @Override
    public boolean test(Object value) {
      return NEGATIVE_NUMBER.test(value) && WHOLE_NUMBER.test(value);
    }
  },
  // 自然数
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
  // 正小数
  POSITIVE_DECIMAL(POSITIVE_NUMBER.types.stream().filter(DECIMAL.types::contains)
      .collect(ImmutableSet.toImmutableSet())) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) && POSITIVE_NUMBER.test(value);
    }
  },
  // 负小数
  NEGATIVE_DECIMAL(NEGATIVE_NUMBER.types.stream().filter(DECIMAL.types::contains)
      .collect(ImmutableSet.toImmutableSet())) {
    @Override
    public boolean test(Object value) {
      return DECIMAL.test(value) && NEGATIVE_NUMBER.test(value);
    }
  },
  // 日期
  DATE(Date.class, Calendar.class, LocalDate.class, String.class) {
    @Override
    public boolean test(Object value) {
      try {
        // TODO
        TypeUtils.castToDate(value);
        return Boolean.TRUE;
      } catch (JSONException e) {
        return Boolean.FALSE;
      }
    }
  },
  // 时间
  TIME(Date.class, Calendar.class, LocalTime.class, String.class) {
    @Override
    public boolean test(Object value) {
      try {
        // TODO
        TypeUtils.castToDate(value);
        return Boolean.TRUE;
      } catch (JSONException e) {
        return Boolean.FALSE;
      }
    }
  },
  // 日期时间
  DATE_TIME(Date.class, Calendar.class, LocalDateTime.class, String.class) {
    @Override
    public boolean test(Object value) {
      try {
        // TODO
        TypeUtils.castToDate(value);
        return Boolean.TRUE;
      } catch (JSONException e) {
        return Boolean.FALSE;
      }
    }
  },
  // 字符串
  STRING(String.class) {
    @Override
    public boolean test(Object t) {
      return true;
    }
  },
  // 单词 TODO include chinese?
  WORD(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(WORD_REGEX);
    }
  },
  // 英文或数字
  ENGLISH_OR_NUMBER(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(ENGLISH_OR_NUMBER_REGEX);
    }
  },
  // 英文
  ENGLISH(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(ENGLISH_REGEX);
    }
  },
  // 中文
  CHINESE(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(CHINESE_REGEX);
    }
  },
  // 邮箱
  EMAIL(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(EMAIL_REGEX);
    }
  },
  // 电话
  PHONE(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(PHONE_REGEX);
    }
  },
  // 手机号码
  TELEPHONE(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(TELEPHONE_REGEX);
    }
  },
  // 身份证号码
  ID_CARD_NUMBER(String.class) {
    @Override
    public boolean test(Object t) {
      return STRING.cast(t, String.class).matches(ID_CARD_NUMBER_REGEX);
    }
  },
  //boolean
  BOOLEAN(boolean.class,Boolean.class,String.class) {
   @Override
   public boolean test(Object t) {
     try {
       TypeUtils.castToBoolean(t);
       return Boolean.TRUE;
     } catch (JSONException e) {
       return Boolean.FALSE;
     }
   }
 },
  //enum
  ENUM(Enum.class,int.class,Integer.class,String.class) {
   @Override
   public boolean test(Object t) {
     return STRING.test(t) || POSITIVE_WHOLE_NUMBER.test(t);
   }
 },
  //natural
  NATURAL() {
   @Override
   public <T> boolean canCast(TypeToken<T> type) {
     return true;
   }
   
   @Override
   public boolean test(Object t) {
     return true;
   }
 };

  private static final String WORD_REGEX = "^(\\w|[\\u0391-\\uFFE5])*$";
  private static final String ENGLISH_OR_NUMBER_REGEX = "^[A-Za-z0-9]*$";
  private static final String ENGLISH_REGEX = "^[A-Za-z]*$";
  private static final String CHINESE_REGEX = "^[\\u0391-\\uFFE5]*$";
  private static final String PHONE_REGEX = "^(\\d{3,4})-\\d{7,8}$";
  private static final String EMAIL_REGEX =
      "^\\w+([-+.]\\w+)*@\\w+([-.]\\\\w+)*\\.\\w+([-.]\\w+)*$";
  private static final String TELEPHONE_REGEX =
      "^1(3[0-9]|4[57]|5[0-35-9]|7[0135678]|8[0-9])\\d{8}$";
  private static final String ID_CARD_NUMBER_REGEX = "(^\\d{15}$)|(^\\d{18}$)|(^\\d{17}(\\d|X|x)$)";
  private final ImmutableCollection<TypeToken<?>> types;

  private BaseDataType(ImmutableCollection<TypeToken<?>> types) {
    this.types = types;
  }

  private BaseDataType(Class<?>... types) {
    this.types = Arrays.stream(types).map(TypeToken::of).collect(ImmutableSet.toImmutableSet());
  }

  public ImmutableCollection<TypeToken<?>> canCastTypes() {
    return types;
  }

  @Override
  public <T> boolean canCast(Class<T> type) {
    return canCast(TypeToken.of(type));
  }

  @Override
  public <T> boolean canCast(TypeToken<T> type) {
    return types.contains(type) || types.stream().anyMatch(type::isSubtypeOf);
  }

  @Override
  public <T> T cast(Object value, Class<T> type) {
    return cast(value, TypeToken.of(type));
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T cast(Object value, TypeToken<T> type) {
    return (T) TypeUtils.castToJavaBean(value, type.getRawType());
  }
}
