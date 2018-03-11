package com.github.liudaomanbu.excel.convert;

import com.google.auto.value.AutoValue;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 *
 * @author liudaomanbu
 * @date 2018-03-10 23:49
 **/
@AutoValue
public abstract class DateTimeFormatterAdapter implements Converter<String,Date>{
  public static DateTimeFormatterAdapter create(DateTimeFormatter newDateTimeFormatter) {
    return new AutoValue_DateTimeFormatterAdapter(newDateTimeFormatter);
  }

  protected abstract DateTimeFormatter getDateTimeFormatter();

  @Override
  public Date convert(String value) {
    try {
      return Date.from(LocalDateTime.from(getDateTimeFormatter().parse(value)).atZone(ZoneId.systemDefault()).toInstant());
    }catch (Exception e){
      throw new ConvertException(value,Date.class,e);
    }
  }

}
