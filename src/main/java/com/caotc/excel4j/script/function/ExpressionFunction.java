package com.caotc.excel4j.script.function;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorObject;

public interface ExpressionFunction  {
  public String getName();

  public AviatorObject call(Map<String, Object> env, AviatorObject... args);
}
