package com.github.liudaomanbu.excel.parse.error;

import org.apache.poi.ss.usermodel.Sheet;

public class SheetValidationError extends ValidationError<Sheet> {

  public SheetValidationError(Sheet cause, String message) {
    super(cause, message);
  }

}
