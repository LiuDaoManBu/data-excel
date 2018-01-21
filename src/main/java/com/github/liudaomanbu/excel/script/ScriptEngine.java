package com.github.liudaomanbu.excel.script;

import java.util.Map;
import com.github.liudaomanbu.excel.script.constant.Option;
import com.github.liudaomanbu.excel.script.expression.ScriptExpression;
import com.github.liudaomanbu.excel.script.runtime.function.ScriptFunction;
import com.googlecode.aviator.AviatorEvaluator;

public class ScriptEngine {

  public static void setOption(Option opt, Object val) {
    opt.setValue(val);
  }

  public static <T> T getOption(Option opt) {
    return opt.getValue();
  }

  public static void clearExpressionCache() {
    AviatorEvaluator.clearExpressionCache();
  }

  public static ScriptClassLoader getExpressionClassLoader() {
    return AviatorTypeUtil.to(AviatorEvaluator.getAviatorClassLoader());
  }

  public static ScriptClassLoader getExpressionClassLoader(boolean cached) {
    return AviatorTypeUtil.to(AviatorEvaluator.getAviatorClassLoader(cached));
  }

  public static void addFunction(ScriptFunction function) {
    AviatorEvaluator.addFunction(AviatorTypeUtil.to(function));
  }

  public static ScriptFunction removeFunction(String name) {
    return AviatorTypeUtil.to(AviatorEvaluator.removeFunction(name));
  }

  public static ScriptFunction removeFunction(ScriptFunction function) {
    return AviatorTypeUtil.to(AviatorEvaluator.removeFunction(AviatorTypeUtil.to(function)));
  }

  public static ScriptFunction getFunction(String name) {
    return AviatorTypeUtil.to(AviatorEvaluator.getFunction(name));
  }

  public static boolean containsFunction(String name) {
    return AviatorEvaluator.containsFunction(name);
  }

  public static ScriptExpression getCachedExpression(String expression) {
    return AviatorTypeUtil.to(AviatorEvaluator.getCachedExpression(expression));
  }

  public static ScriptExpression compile(final String expression, final boolean cached) {
    return AviatorTypeUtil.to(AviatorEvaluator.compile(expression, cached));
  }

  public static ScriptExpression compile(String expression) {
    return AviatorTypeUtil.to(AviatorEvaluator.compile(expression));
  }

  public static Object execute(String expression, Object... values) {
    return AviatorEvaluator.exec(expression, values);
  }

  public static Object execute(String expression, Map<String, Object> env, boolean cached) {
    return AviatorEvaluator.execute(expression, env, cached);
  }

  public static Object execute(String expression, Map<String, Object> env) {
    return AviatorEvaluator.execute(expression, env);
  }

  public static Object execute(String expression) {
    return AviatorEvaluator.execute(expression);
  }

  public static void removeCache(String expression) {
    AviatorEvaluator.invalidateCache(expression);
  }
}
