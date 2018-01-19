package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Sheet;

public class SheetError extends ConstraintViolation<Sheet> {

  public SheetError(Sheet cause, String message) {
    super(cause, message);
  }

}
