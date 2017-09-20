package com.caotc.excel4j.script;

import java.util.Map;
import com.caotc.excel4j.script.constant.Option;
import com.caotc.excel4j.script.expression.AviatorExpression;
import com.caotc.excel4j.script.expression.Expression;
import com.caotc.excel4j.script.function.AviatorExpressionFunction;
import com.caotc.excel4j.script.function.ExpressionAviatorFunction;
import com.caotc.excel4j.script.function.ExpressionFunction;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.runtime.type.AviatorFunction;

public class ExpressionEngine {

  public static void setOption(Option opt, Object val) {
    opt.setValue(val);
  }

  public static <T> T getOption(Option opt) {
    return opt.getValue();
  }

  public static void clearExpressionCache() {
    AviatorEvaluator.clearExpressionCache();
  }

  public static ExpressionClassLoader getExpressionClassLoader() {
    return AviatorUtil.valueOf(AviatorEvaluator.getAviatorClassLoader());
  }

  public static ExpressionClassLoader getExpressionClassLoader(boolean cached) {
    return AviatorUtil.valueOf(AviatorEvaluator.getAviatorClassLoader(cached));
  }

  public static void addFunction(ExpressionFunction function) {
    AviatorEvaluator.addFunction(AviatorUtil.valueOf(function));
  }

  public static ExpressionFunction removeFunction(String name) {
    return AviatorUtil.valueOf(AviatorEvaluator.removeFunction(name));
  }

  public static ExpressionFunction removeFunction(ExpressionFunction function) {
    return AviatorUtil.valueOf(AviatorEvaluator.removeFunction(AviatorUtil.valueOf(function)));
  }

  public static ExpressionFunction getFunction(String name) {
    return AviatorUtil.valueOf(AviatorEvaluator.getFunction(name));
  }

  public static boolean containsFunction(String name) {
    return AviatorEvaluator.containsFunction(name);
  }

  public static Expression getCachedExpression(String expression) {
    return AviatorUtil.valueOf(AviatorEvaluator.getCachedExpression(expression));
  }

  public static Expression compile(final String expression, final boolean cached) {
    return AviatorUtil.valueOf(AviatorEvaluator.compile(expression, cached));
  }

  public static Expression compile(String expression) {
    return AviatorUtil.valueOf(AviatorEvaluator.compile(expression));
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

  private static class AviatorUtil {
    public static ExpressionFunction valueOf(AviatorFunction function) {
      return AviatorExpressionFunction.valueOf(function);
    }

    public static AviatorFunction valueOf(ExpressionFunction function) {
      return ExpressionAviatorFunction.valueOf(function);
    }
    
    public static ExpressionClassLoader valueOf(AviatorClassLoader classLoader) {
      return AviatorExpressionClassLoader.valueOf(classLoader);
    }
    
    public static Expression valueOf(com.googlecode.aviator.Expression expression) {
      return AviatorExpression.valueOf(expression);
    }
  }
}
