package com.github.liudaomanbu.excel.matcher.data.type;

import com.google.common.collect.ImmutableMap;
import java.util.function.Function;
import java.util.function.Predicate;
import org.apache.poi.ss.usermodel.Cell;

public enum CellDataType implements Predicate<Object>{
  BOOLEAN {
    @Override
    public boolean test(Object t) {
      if(t instanceof String) {
        String string=(String) t;
        return "Y".equalsIgnoreCase(string) || "N".equalsIgnoreCase(string) || "YES".equalsIgnoreCase(string) 
            || "NO".equalsIgnoreCase(string) || "是".equals(string) || "否".equals(string);
      }
      if(DOUBLE.test(t)) {
        
      }
      return t instanceof Boolean;
    }
  },STRING {
    @Override
    public boolean test(Object t) {
      return true;
    }
  },DATE {
    @Override
    public boolean test(Object t) {
      return false;
    }
  },DOUBLE {
    @Override
    public boolean test(Object t) {
      return false;
    }
  };
}
