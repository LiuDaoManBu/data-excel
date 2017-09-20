package com.caotc.excel4j.expression;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;

public class ExpressionEngine {

  public static void setOption(Options opt, Object val) {
    AviatorEvaluator.setOption(opt, val);
  }

  public static <T> T getOption(Options opt) {
    return AviatorEvaluator.getOption(opt);
  }

  public static void clearExpressionCache() {
    AviatorEvaluator.clearExpressionCache();
  }

  public static ExpressionClassLoader getExpressionClassLoader() {
    return ExpressionClassLoader.valueOf(AviatorEvaluator.getAviatorClassLoader());
  }

  public static ExpressionClassLoader getExpressionClassLoader(boolean cached) {
    return ExpressionClassLoader.valueOf(AviatorEvaluator.getAviatorClassLoader(cached));
  }

  public static void addFunction(ExpressionFunction function) {
    AviatorEvaluator.addFunction(function);
  }

  public static ExpressionFunction removeFunction(String name) {
    return BaseExpressionFunction.valueOf(AviatorEvaluator.removeFunction(name));
  }

  public static ExpressionFunction removeFunction(ExpressionFunction function) {
    return BaseExpressionFunction.valueOf(AviatorEvaluator.removeFunction(function));
  }

  public static ExpressionFunction getFunction(String name) {
    return BaseExpressionFunction.valueOf(AviatorEvaluator.getFunction(name));
  }

  public static boolean containsFunction(String name) {
    return AviatorEvaluator.containsFunction(name);
  }

  public static Expression getCachedExpression(String expression) {
    return BaseExpression.valueOf(AviatorEvaluator.getCachedExpression(expression));
  }

  public static Expression compile(final String expression, final boolean cached) {
    return BaseExpression.valueOf(AviatorEvaluator.compile(expression, cached));
  }

  public static Expression compile(String expression) {
    return BaseExpression.valueOf(AviatorEvaluator.compile(expression));
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
