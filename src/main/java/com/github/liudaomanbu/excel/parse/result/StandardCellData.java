package com.github.liudaomanbu.excel.parse.result;

import com.google.common.reflect.TypeToken;

public class StandardCellData<T> {
  private final StandardCell cell;
  private final Menu<T> menu;
  private final Object value;

  public StandardCellData() {
    cell = null;
    menu = null;
    value = menu.getConfig().getDataConfig().cast(cell.getValue(),
        TypeToken.of(menu.getField().getType()));
  }

  public StandardCell getCell() {
    return cell;
  }

  public Menu<T> getMenu() {
    return menu;
  }

  public Object getValue() {
    return value;
  }
}
