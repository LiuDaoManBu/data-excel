package com.github.liudaomanbu.excel.matcher.data.type;

import java.util.Date;
import com.github.liudaomanbu.excel.config.ParserConfig;

public enum CellDataType{
  BOOLEAN {
    @Override
    public boolean matches(Boolean value, ParserConfig config) {
      return true;
    }

    @Override
    public boolean matches(String value, ParserConfig config) {
      // TODO Auto-generated method stub
      return false;
    }
  },STRING {
    @Override
    public boolean matches(Boolean value, ParserConfig config) {
      return true;
    }

    @Override
    public boolean matches(String value, ParserConfig config) {
      // TODO Auto-generated method stub
      return false;
    }
  },DATE {
    @Override
    public boolean matches(Boolean value, ParserConfig config) {
      return false;
    }

    @Override
    public boolean matches(String value, ParserConfig config) {
      // TODO Auto-generated method stub
      return false;
    }
  },DOUBLE {
    @Override
    public boolean matches(Boolean value, ParserConfig config) {
      return false;
    }

    @Override
    public boolean matches(String value, ParserConfig config) {
      // TODO Auto-generated method stub
      return false;
    }
  };
  public abstract boolean matches(Boolean value,ParserConfig config);
  public abstract boolean matches(String value,ParserConfig config);
  public  boolean matches(Double value,ParserConfig config) {return false;};
  public  boolean matches(Date value,ParserConfig config) {return false;};
}
