package com.github.liudaomanbu.excel.matcher.data.type;

import java.util.function.Predicate;

public enum CellDataType implements Predicate<Object>{
  BOOLEAN {
    @Override
    public boolean test(Object t) {
      return false;
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
  public abstract boolean test(Object t);
}
