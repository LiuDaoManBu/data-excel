package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.MenuData;

public class DataError<V> extends Error<MenuData<V>> {

  public DataError(MenuData<V> cause, String message) {
    super(cause, message);
  }

}