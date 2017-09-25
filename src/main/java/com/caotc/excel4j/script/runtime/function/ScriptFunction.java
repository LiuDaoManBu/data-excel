package com.caotc.excel4j.script.runtime.function;

import java.util.Map;
import com.caotc.excel4j.script.runtime.type.ScriptObject;

public interface ScriptFunction  {
  public String getName();

  public ScriptObject call(Map<String, Object> env, ScriptObject... args);
}
