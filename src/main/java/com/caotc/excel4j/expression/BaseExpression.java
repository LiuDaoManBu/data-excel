package com.caotc.excel4j.expression;

import java.util.List;
import java.util.Map;

public class BaseExpression implements Expression {
  public static BaseExpression valueOf(com.googlecode.aviator.Expression expression) {
    return new BaseExpression(expression);
  }
  
  private com.googlecode.aviator.Expression expression;
  
  private BaseExpression(com.googlecode.aviator.Expression expression) {
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
