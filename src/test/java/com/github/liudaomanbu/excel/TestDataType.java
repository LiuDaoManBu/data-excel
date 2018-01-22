package com.github.liudaomanbu.excel;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;
import com.alibaba.fastjson.util.TypeUtils;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.matcher.data.type.DataType;
import com.google.common.reflect.TypeToken;

public class TestDataType {
  public static void main(String[] args) {
    System.out.println(LocalDate.now().toString());
  }
  
  public static void testDate() {
    DataType dataType= BaseDataType.DATE;
    System.out.println(dataType.test(new Date()));
    System.out.println(dataType.test(new Date().toString()));
    System.out.println(dataType.test(2018));
    System.out.println(dataType.cast("2018-01-01",Date.class));
  }
}
