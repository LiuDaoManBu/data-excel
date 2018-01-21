package com.github.liudaomanbu.excel.parse.error;

import com.github.liudaomanbu.excel.parse.result.Table;

public class TableValidationError extends ValidationError<Table> {

  public TableValidationError(Table cause, String message) {
    super(cause, message);
  }

}
