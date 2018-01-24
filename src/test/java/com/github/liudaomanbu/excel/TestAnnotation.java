package com.github.liudaomanbu.excel;

import java.util.stream.IntStream;
import com.github.liudaomanbu.excel.annotation.ExcelField;
import com.github.liudaomanbu.excel.annotation.ExcelMenu;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

class Bean{
  @ExcelField(menus= {@ExcelMenu(value = "aaa")})
  private String name;
  @ExcelField(menus= {@ExcelMenu(value = "aaa")})
  private String value;
}

public class TestAnnotation {
  public static void main(String[] args) throws NoSuchFieldException, SecurityException {
    Multimap<Object, Object> idTochildrenIds=HashMultimap.create();
    idTochildrenIds.put(new Object(), null);
    System.out.println(idTochildrenIds);
  }
  
  public static void testAnnotationEquals() throws NoSuchFieldException, SecurityException {
    ExcelField excelField1=Bean.class.getDeclaredField("name").getAnnotation(ExcelField.class);
    ExcelField excelField2=Bean.class.getDeclaredField("value").getAnnotation(ExcelField.class);
    System.out.println(excelField1.equals(excelField2));
    
    ExcelMenu[] menus1=excelField1.menus();
    ExcelMenu[] menus2=excelField2.menus();
    System.out.println(menus1.equals(menus2));
    
    System.out.println(menus1[0].equals(menus2[0]));
  }
}
