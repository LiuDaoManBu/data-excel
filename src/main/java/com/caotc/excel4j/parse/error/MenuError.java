package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Menu;

public class MenuError<V> extends Error<Menu<V>> {

  public MenuError(Menu<V> cause, String message) {
    super(cause, message);
  }
}
