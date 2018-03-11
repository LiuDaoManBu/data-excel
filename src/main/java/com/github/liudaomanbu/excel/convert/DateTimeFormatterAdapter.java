package com.github.liudaomanbu.excel.convert;

import com.google.common.base.Function;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Objects;

/**
 *
 * @author: liudaomanbu
 * @create: 2018-03-10 23:49
 **/
public class DateTimeFormatterAdapter implements Converter<String,Date>{
  private final DateTimeFormatter dateTimeFormatter;

  public static DateTimeFormatterAdapter create(DateTimeFormatter dateTimeFormatter){
    return new DateTimeFormatterAdapter(dateTimeFormatter);
  }

  private DateTimeFormatterAdapter(DateTimeFormatter dateTimeFormatter) {
    this.dateTimeFormatter = dateTimeFormatter;
  }

  @Override
  public Date convert(String value) {
    try {
      return Date.from(LocalDateTime.from(dateTimeFormatter.parse(value)).atZone(ZoneId.systemDefault()).toInstant());
    }catch (Exception e){
      throw new ConvertException(value,Date.class,e);
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DateTimeFormatterAdapter that = (DateTimeFormatterAdapter) o;
    return Objects.equals(dateTimeFormatter, that.dateTimeFormatter);
  }

  @Override
  public int hashCode() {

    return Objects.hash(dateTimeFormatter);
  }

  @Override
  public String toString() {
    return "DateTimeFormatterAdapter{" +
        "dateTimeFormatter=" + dateTimeFormatter +
        '}';
  }
}
