package com.caotc.excel4j.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.Necessity;
import com.caotc.excel4j.matcher.data.type.BaseDataType;

@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface ExcelField {
  String name();

  // enum 不能使用其他类常量?
  Direction direction() default Direction.BOTTOM;

  int distance() default MenuConfig.DEFAULT_DISTANCE;

  Necessity necessity() default Necessity.MUST;

  LoadType loadType() default LoadType.UNFIXED;

  int dataNumber() default 0;

  BaseDataType dataType();
  // String dateformat() default "yyyy-MM-dd HH:mm:ss";
  // int width() default 0;

  // HorizontalAlignment align() default HorizontalAlignment.LEFT;
}
