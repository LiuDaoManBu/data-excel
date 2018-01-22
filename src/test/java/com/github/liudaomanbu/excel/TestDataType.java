package com.github.liudaomanbu.excel;

import java.util.Date;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.matcher.data.type.DataType;

public class TestDataType {
  public static void main(String[] args) {
    testDate();
  }
  
  public static void testDate() {
    DataType dataType= BaseDataType.DATE;
    System.out.println(dataType.test(new Date()));
    System.out.println(dataType.test(new Date().toString()));
    System.out.println(dataType.test("2018"));
    System.out.println(dataType.cast("2018-01-01",Date.class));
  }
}
