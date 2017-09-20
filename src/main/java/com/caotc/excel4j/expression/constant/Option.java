package com.caotc.excel4j.expression.constant;

import java.math.MathContext;
import java.util.concurrent.ConcurrentHashMap;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;

public enum Option {
  DEFAULT_DECIMAL_TYPE(Type.DOUBLE) {
    @Override
    public boolean isValidValue(Object val) {
      return val instanceof Type && (Type.DOUBLE.equals(val) || Type.DECIMAL.equals(val));
    }

    @Override
    void doSetValue(Object val) {
      AviatorEvaluator.setOption(Options.ALWAYS_USE_DOUBLE_AS_DECIMAL,
          Type.DECIMAL.equals((Type) val));
    }
  },
  MATH_CONTEXT(MathContext.DECIMAL128) {
    @Override
    public boolean isValidValue(Object val) {
      return val instanceof MathContext;
    }

    @Override
    void doSetValue(Object val) {
      AviatorEvaluator.setOption(Options.MATH_CONTEXT, (MathContext) val);
    }
  };
  private static final ConcurrentHashMap<Option, Object> OPTIONS =
      new ConcurrentHashMap<Option, Object>();
  
  private Option(Object defaultValue) {
    this.defaultValue = defaultValue;
    doSetValue(defaultValue);
  }

  private final Object defaultValue;

  public abstract boolean isValidValue(Object val);

  public Object getDefaultValue() {
    return defaultValue;
  }

  public void setValue(Object val) {
    if (!isValidValue(val)) {
      throw new IllegalArgumentException(val + "is invalid value for option:" + name());
    }
    doSetValue(val);
    OPTIONS.put(this, val);
  }

  @SuppressWarnings("unchecked")
  public <T> T getValue() {
    return (T) OPTIONS.getOrDefault(this, getDefaultValue());
  }

  abstract void doSetValue(Object val);
}
