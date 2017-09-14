package com.caotc.excel4j.parse.result;

import org.apache.poi.ss.usermodel.Cell;
import com.caotc.excel4j.util.ExcelUtil;

public class CellData {
  private Menu menu;
  private Cell valueCell;

  public Object getValue() {
    return ExcelUtil.getValue(valueCell);
  }

  @SuppressWarnings("unchecked")
  public <T> T getValue(Class<T> type) {
    Object value = getValue();
    if (value != null && !value.getClass().equals(type)) {
      value = menu.getCheckMenuConfig().getDataMatcher().getDataType().cast(value, type);
    }
    return (T) value;
  }

  public Menu getMenu() {
    return menu;
  }

  public void setMenu(Menu menu) {
    this.menu = menu;
  }

  public Cell getValueCell() {
    return valueCell;
  }

  public void setValueCell(Cell valueCell) {
    this.valueCell = valueCell;
  }
}
