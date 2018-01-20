package com.caotc.excel4j.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import com.caotc.excel4j.matcher.constant.StringMatcherType;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ExcelSheet {
  String value() default "";

  StringMatcherType valueMatcherType() default StringMatcherType.EQUALS;
}

