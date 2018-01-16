package com.caotc.excel4j.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface ExcelSheet {
  String name() default "";

  // HSSFColor.HSSFColorPredefined headColor() default HSSFColor.HSSFColorPredefined.LIGHT_GREEN;

}

