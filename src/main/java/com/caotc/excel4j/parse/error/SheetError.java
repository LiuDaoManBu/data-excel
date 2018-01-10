package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Sheet;

public class SheetError {
  private Sheet sheet;
  private String message;

  public SheetError(Sheet sheet, String message) {
    super();
    this.sheet = sheet;
    this.message = message;
  }

  public Sheet getSheet() {
    return sheet;
  }

  public void setSheet(Sheet sheet) {
    this.sheet = sheet;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }
}
