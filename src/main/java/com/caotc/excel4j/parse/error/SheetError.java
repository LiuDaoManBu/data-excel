package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Sheet;

public class SheetError extends Error<Sheet> {

  public SheetError(Sheet cause, String message) {
    super(cause, message);
  }

}
