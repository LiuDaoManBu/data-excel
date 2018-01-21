package com.github.liudaomanbu.excel;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;

public class TestAviator {

  public static void main(String[] args) {
    String expression = "(data>1) && ((data<2) || (data==6 && data>8)) && (data!=nil)";
    // 编译表达式
    Expression compiledExp = AviatorEvaluator.compile(expression);
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("data", 4);
    // 执行表达式
    Boolean result = (Boolean) compiledExp.execute(env);
    System.out.println(result);  // false  
  }
}
