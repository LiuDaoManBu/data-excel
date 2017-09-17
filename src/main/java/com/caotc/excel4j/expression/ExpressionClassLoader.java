package com.caotc.excel4j.expression;

import com.googlecode.aviator.parser.AviatorClassLoader;

public class ExpressionClassLoader extends ClassLoader {
  private AviatorClassLoader classLoader;

  public static ExpressionClassLoader valueOf(ClassLoader classLoader) {
    return new ExpressionClassLoader(classLoader);
  }

  private ExpressionClassLoader(ClassLoader parent) {
    super(parent);
    this.classLoader = new AviatorClassLoader(parent);
  }

  public Class<?> defineClass(String name, byte[] b) {
    return classLoader.defineClass(name, b);
  }

}
