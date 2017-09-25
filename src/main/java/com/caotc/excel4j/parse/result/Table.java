package com.caotc.excel4j.parse.result;

import java.util.List;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.parse.error.SheetError;

public class Table {
  private TableConfig tableConfig;
  private List<SheetError> sheetErrors;
  private SheetParseResult sheetParseResult;
  private Direction fixedMenuDirection;
  private Direction unFixedMenuDirection;
  public TableConfig getTableConfig() {
    return tableConfig;
  }
  public void setTableConfig(TableConfig tableConfig) {
    this.tableConfig = tableConfig;
  }
  public List<SheetError> getSheetErrors() {
    return sheetErrors;
  }
  public void setSheetErrors(List<SheetError> sheetErrors) {
    this.sheetErrors = sheetErrors;
  }
  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }
  public void setSheetParseResult(SheetParseResult sheetParseResult) {
    this.sheetParseResult = sheetParseResult;
  }
  public Direction getFixedMenuDirection() {
    return fixedMenuDirection;
  }
  public void setFixedMenuDirection(Direction fixedMenuDirection) {
    this.fixedMenuDirection = fixedMenuDirection;
  }
  public Direction getUnFixedMenuDirection() {
    return unFixedMenuDirection;
  }
  public void setUnFixedMenuDirection(Direction unFixedMenuDirection) {
    this.unFixedMenuDirection = unFixedMenuDirection;
  }
  
}
