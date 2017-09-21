package com.caotc.excel4j.script.constant;

import java.util.EnumSet;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import com.google.common.collect.ImmutableMap;
import com.googlecode.aviator.runtime.type.AviatorType;

public enum ScriptType {
  DOUBLE {
    @Override
    public AviatorType to() {
      return AviatorType.Double;
    }
  },
  DECIMAL {
    @Override
    public AviatorType to() {
      return AviatorType.Decimal;
    }
  },
  LONG {
    @Override
    public AviatorType to() {
      return AviatorType.Long;
    }
  },
  STRING {
    @Override
    public AviatorType to() {
      return AviatorType.String;
    }
  },
  JAVATYPE {
    @Override
    public AviatorType to() {
      return AviatorType.JavaType;
    }
  },
  BOOLEAN {
    @Override
    public AviatorType to() {
      return AviatorType.Boolean;
    }
  },
  PATTERN {
    @Override
    public AviatorType to() {
      return AviatorType.Pattern;
    }
  },
  EMTPY {
    @Override
    public AviatorType to() {
      return AviatorType.Nil;
    }
  },
  METHOD {
    @Override
    public AviatorType to() {
      return AviatorType.Method;
    }
  },
  BIGINT {
    @Override
    public AviatorType to() {
      return AviatorType.BigInt;
    }
  };

  private static final Map<AviatorType, ScriptType> AVIATOR_TYPE_TO_SCRIPT_TYPES =
      ImmutableMap.copyOf(EnumSet.allOf(ScriptType.class).stream()
          .collect(Collectors.toMap(ScriptType::to, Function.identity())));

  public abstract AviatorType to();

  public static ScriptType from(AviatorType aviatorType) {
    return AVIATOR_TYPE_TO_SCRIPT_TYPES.get(aviatorType);
  }
}
