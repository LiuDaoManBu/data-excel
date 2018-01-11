package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Table;

public class TableError {
  private Table table;
  private String message;

  public TableError(Table table, String message) {
    super();
    this.table = table;
    this.message = message;
  }

  public Table getTable() {
    return table;
  }

  public void setTable(Table table) {
    this.table = table;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

}
