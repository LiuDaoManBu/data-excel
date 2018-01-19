package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Workbook;

public class WorkbookValidationError extends ValidationError<Workbook> {

  public WorkbookValidationError(Workbook cause, String message) {
    super(cause, message);
  }

}
