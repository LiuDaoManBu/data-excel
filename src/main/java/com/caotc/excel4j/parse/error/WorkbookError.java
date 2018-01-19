package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Workbook;

public class WorkbookError extends ValidationError<Workbook> {

  public WorkbookError(Workbook cause, String message) {
    super(cause, message);
  }

}
