package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.StandardCell;

public class StandardCellError extends ValidationError<StandardCell> {

  public StandardCellError(StandardCell cause, String message) {
    super(cause, message);
  }
}
