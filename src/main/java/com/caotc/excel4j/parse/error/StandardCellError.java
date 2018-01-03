package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.StandardCell;

public class StandardCellError {
  private StandardCell cell;
  private String errorMessage;

  public StandardCellError(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public StandardCellError(StandardCell cell, String errorMessage) {
    this.cell = cell;
    this.errorMessage = errorMessage;
  }

  public StandardCell getCell() {
    return cell;
  }

  public void setCell(StandardCell cell) {
    this.cell = cell;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }
}
