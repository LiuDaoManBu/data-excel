package com.github.liudaomanbu.excel.convert;

import com.google.common.base.Function;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 *
 * @author: liudaomanbu
 * @create: 2018-03-10 23:49
 **/
public class DateTimeFormatterAdapter implements Function<String,Date>{
  private final DateTimeFormatter dateTimeFormatter;

  public static DateTimeFormatterAdapter create(DateTimeFormatter dateTimeFormatter){
    return new DateTimeFormatterAdapter(dateTimeFormatter);
  }

  private DateTimeFormatterAdapter(DateTimeFormatter dateTimeFormatter) {
    this.dateTimeFormatter = dateTimeFormatter;
  }

  @Override
  public Date apply(String input) {
    return Date.from(LocalDateTime.from(dateTimeFormatter.parse(input)).atZone(ZoneId.systemDefault()).toInstant());
  }

}
