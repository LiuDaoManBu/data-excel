package com.github.liudaomanbu.excel.parse.error;

import com.github.liudaomanbu.excel.parse.result.StandardCell;

public class StandardCellValidationError extends ValidationError<StandardCell> {

  public StandardCellValidationError(StandardCell cause, String message) {
    super(cause, message);
  }
}
