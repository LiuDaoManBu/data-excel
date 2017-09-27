package com.caotc.excel4j.parse.result;

import java.util.List;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.parse.error.SheetError;
import com.caotc.excel4j.parse.error.TableError;

public class Table {
  private TableConfig tableConfig;
  private List<TableError> errors;
  private SheetParseResult sheetParseResult;
  
  public TableConfig getTableConfig() {
    return tableConfig;
  }
  public void setTableConfig(TableConfig tableConfig) {
    this.tableConfig = tableConfig;
  }

  public List<TableError> getErrors() {
    return errors;
  }
  public void setErrors(List<TableError> errors) {
    this.errors = errors;
  }
  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }
  public void setSheetParseResult(SheetParseResult sheetParseResult) {
    this.sheetParseResult = sheetParseResult;
  }
  
}
