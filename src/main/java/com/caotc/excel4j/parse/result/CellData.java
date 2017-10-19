package com.caotc.excel4j.parse.result;

public class CellData {
  private final Menu menu;
  private final StandardCell valueCell;

  public CellData(Menu menu, StandardCell valueCell) {
    super();
    this.menu = menu;
    this.valueCell = valueCell;
  }

  public Object getValue() {
    return valueCell.getValue();
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

  public StandardCell getValueCell() {
    return valueCell;
  }
}
