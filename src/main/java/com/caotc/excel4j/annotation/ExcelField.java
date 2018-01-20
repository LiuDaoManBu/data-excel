package com.caotc.excel4j.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.matcher.data.type.BaseDataType;

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface ExcelField {
  BaseDataType dataType() default BaseDataType.STRING;

  LoadType loadType() default LoadType.UNFIXED;

  ExcelMenu menu();

  ExcelMenu[] superMenus() default {};
}
