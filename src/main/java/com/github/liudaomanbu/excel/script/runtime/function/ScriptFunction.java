package com.github.liudaomanbu.excel.script.runtime.function;

import java.util.Map;
import com.github.liudaomanbu.excel.script.runtime.type.BaseScriptObject;

public interface ScriptFunction  {
  public String getName();

  public BaseScriptObject call(Map<String, Object> env, BaseScriptObject... args);
}
