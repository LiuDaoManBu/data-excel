package com.caotc.excel4j.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.Necessity;
import com.caotc.excel4j.matcher.constant.StringMatcherType;

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
