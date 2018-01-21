package com.github.liudaomanbu.excel.parse.error;

import com.github.liudaomanbu.excel.parse.result.Menu;

public class MenuValidationError extends ValidationError<Menu> {

  public MenuValidationError(Menu cause, String message) {
    super(cause, message);
  }
}
