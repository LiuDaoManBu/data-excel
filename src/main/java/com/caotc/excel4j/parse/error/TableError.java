package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Table;

public class TableError extends ValidationError<Table> {

  public TableError(Table cause, String message) {
    super(cause, message);
  }

}
