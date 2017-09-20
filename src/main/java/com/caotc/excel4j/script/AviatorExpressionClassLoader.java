package com.caotc.excel4j.script;

import com.googlecode.aviator.parser.AviatorClassLoader;

public class AviatorExpressionClassLoader extends ExpressionClassLoader {
  private AviatorClassLoader classLoader;

  public static AviatorExpressionClassLoader valueOf(AviatorClassLoader classLoader) {
    return new AviatorExpressionClassLoader(classLoader);
  }
  
  private AviatorExpressionClassLoader(AviatorClassLoader classLoader) {
    super(classLoader.getParent());
    this.classLoader = classLoader;
  }

  public AviatorExpressionClassLoader(ClassLoader parent) {
    super(parent);
    this.classLoader = new AviatorClassLoader(parent);
  }

  public Class<?> defineClass(String name, byte[] b) {
    return classLoader.defineClass(name, b);
  }

}
