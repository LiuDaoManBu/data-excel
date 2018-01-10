package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Workbook;

public class WorkbookError {
  private Workbook workbook;
  private String message;

  public WorkbookError(Workbook workbook, String message) {
    super();
    this.workbook = workbook;
    this.message = message;
  }

  public Workbook getWorkbook() {
    return workbook;
  }

  public void setWorkbook(Workbook workbook) {
    this.workbook = workbook;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

}
