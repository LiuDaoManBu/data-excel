package com.github.liudaomanbu.excel.convert;

/**
 * @author: liudaomanbu
 * @create: 2018-03-11 20:47
 **/
public class ConvertException extends IllegalArgumentException{
  private static String generateMessage(Object value,Class<?> targetClass){
    return value+"  can't convert to "+targetClass;
  }

  private final Object value;
  private final Class<?> targetClass;

  public ConvertException(Object value, Class<?> targetClass) {
    super(generateMessage(value,targetClass));
    this.value = value;
    this.targetClass = targetClass;
  }

  public ConvertException(Object value, Class<?> targetClass,Throwable cause) {
    super(generateMessage(value,targetClass),cause);
    this.value = value;
    this.targetClass = targetClass;
  }

  public Object getValue() {
    return value;
  }

  public Class<?> getTargetClass() {
    return targetClass;
  }
}
