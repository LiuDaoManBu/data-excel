package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Sheet;

public class SheetValidationError extends ValidationError<Sheet> {

  public SheetValidationError(Sheet cause, String message) {
    super(cause, message);
  }

}
