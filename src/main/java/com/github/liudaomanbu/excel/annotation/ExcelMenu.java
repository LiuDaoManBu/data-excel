package com.github.liudaomanbu.excel.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import com.github.liudaomanbu.excel.config.MenuConfig;
import com.github.liudaomanbu.excel.constant.Direction;
import com.github.liudaomanbu.excel.constant.Necessity;
import com.github.liudaomanbu.excel.matcher.constant.StringMatcherType;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ExcelMenu {
  String value();

  StringMatcherType valueMatcherType() default StringMatcherType.EQUALS;

  // enum 不能使用其他类常量?
  Direction direction() default Direction.BOTTOM;

  int distance() default MenuConfig.DEFAULT_DISTANCE;

  Necessity necessity() default Necessity.MUST;
}
