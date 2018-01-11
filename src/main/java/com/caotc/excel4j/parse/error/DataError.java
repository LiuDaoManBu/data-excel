package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.formula.functions.T;
import com.caotc.excel4j.parse.result.Data;

public class DataError<V> {
  private Data<V> menu;
  private String message;

  public DataError(Data<V> menu, String message) {
    super();
    this.menu = menu;
    this.message = message;
  }

  public Data<V> getMenu() {
    return menu;
  }

  public void setMenu(Data<V> menu) {
    this.menu = menu;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

}
