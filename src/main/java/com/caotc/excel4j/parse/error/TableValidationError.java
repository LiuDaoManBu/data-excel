package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Table;

public class TableValidationError extends ValidationError<Table> {

  public TableValidationError(Table cause, String message) {
    super(cause, message);
  }

}
