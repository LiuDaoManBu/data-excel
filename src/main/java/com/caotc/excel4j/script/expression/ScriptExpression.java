package com.caotc.excel4j.script.expression;

import java.util.List;
import java.util.Map;

public interface ScriptExpression {
  Object execute(Map<String, Object> env);

  Object execute();

  List<String> getVariableNames();

  List<String> getVariableFullNames();

}
