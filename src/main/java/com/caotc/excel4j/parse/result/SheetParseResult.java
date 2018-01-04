package com.caotc.excel4j.parse.result;

import java.util.List;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.SheetError;

public class SheetParseResult {
  public static SheetParseResult parse() {
    return new SheetParseResult();
  }
  
  private Sheet sheet;
  private SheetConfig sheetConfig;
  private List<SheetError> sheetErrors;
  private List<Table> tables;
  private WorkbookParseResult workbookParseResult;

  public SheetParseResult(Sheet sheet, SheetConfig sheetConfig) {
    this.sheet = sheet;
    this.sheetConfig = sheetConfig;
  }

  public boolean hasError() {
    return !sheetErrors.isEmpty();
  }

  public SheetError getFirstError() {
    return sheetErrors.get(0);
  }

  public boolean addError(SheetError sheetError) {
    if (!sheetErrors.contains(sheetError)) {
      return sheetErrors.add(sheetError);
    }
    return Boolean.FALSE;
  }

  public SheetError addError(String errorMessage) {
    SheetError error = new SheetError(errorMessage);
    addError(error);
    return error;
  }

  public List<SheetError> getErrors() {
    return sheetErrors;
  }

  public void setErrors(List<SheetError> errors) {
    this.sheetErrors = errors;
  }

  public Sheet getSheet() {
    return sheet;
  }

  public void setSheet(Sheet sheet) {
    this.sheet = sheet;
  }

}
