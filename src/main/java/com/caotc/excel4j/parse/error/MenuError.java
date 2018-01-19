package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Menu;

public class MenuError extends ValidationError<Menu> {

  public MenuError(Menu cause, String message) {
    super(cause, message);
  }
}
