package com.caotc.excel4j.script.expression;

import java.util.List;
import java.util.Map;

public class AviatorExpression implements Expression, com.googlecode.aviator.Expression {
  public static AviatorExpression valueOf(com.googlecode.aviator.Expression expression) {
    return new AviatorExpression(expression);
  }

  private com.googlecode.aviator.Expression expression;

  private AviatorExpression(com.googlecode.aviator.Expression expression) {
    super();
    this.expression = expression;
  }

  public Object execute(Map<String, Object> env) {
    return expression.execute(env);
  }

  public Object execute() {
    return expression.execute();
  }

  public List<String> getVariableNames() {
    return expression.getVariableNames();
  }

  public List<String> getVariableFullNames() {
    return expression.getVariableFullNames();
  }

}
