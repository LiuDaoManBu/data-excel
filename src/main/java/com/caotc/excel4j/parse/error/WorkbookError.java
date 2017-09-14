package com.caotc.excel4j.parse.error;

import org.apache.poi.ss.usermodel.Workbook;

public class WorkbookError {
  private Workbook workbook;
  private String errorMessage;

  public WorkbookError(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public WorkbookError(Workbook workbook, String errorMessage) {
    this.workbook = workbook;
    this.errorMessage = errorMessage;
  }

  public Workbook getSheet() {
    return workbook;
  }

  public void setSheet(Workbook workbook) {
    this.workbook = workbook;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }
}
