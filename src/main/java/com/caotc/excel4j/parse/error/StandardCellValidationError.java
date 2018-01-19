package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.StandardCell;

public class StandardCellValidationError extends ValidationError<StandardCell> {

  public StandardCellValidationError(StandardCell cause, String message) {
    super(cause, message);
  }
}
