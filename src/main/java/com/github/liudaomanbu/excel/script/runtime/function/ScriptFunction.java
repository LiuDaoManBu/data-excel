package com.github.liudaomanbu.excel.script.runtime.function;

import java.util.Map;
import com.github.liudaomanbu.excel.script.runtime.type.ScriptObject;

public interface ScriptFunction  {
  public String getName();

  public ScriptObject call(Map<String, Object> env, ScriptObject... args);
}
