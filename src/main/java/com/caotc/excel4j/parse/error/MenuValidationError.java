package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Menu;

public class MenuValidationError extends ValidationError<Menu> {

  public MenuValidationError(Menu cause, String message) {
    super(cause, message);
  }
}
