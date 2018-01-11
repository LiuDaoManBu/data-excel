package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Menu;

public class MenuError<V> {
  private Menu<V> menu;
  private String message;

  public MenuError(Menu<V> menu, String message) {
    super();
    this.menu = menu;
    this.message = message;
  }

  public Menu<V> getMenu() {
    return menu;
  }

  public void setMenu(Menu<V> menu) {
    this.menu = menu;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }
}
