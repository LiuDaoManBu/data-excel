package com.caotc.excel4j.parse.error;

import com.caotc.excel4j.parse.result.Data;

public class DataError<V> extends Error<Data<V>> {

  public DataError(Data<V> cause, String message) {
    super(cause, message);
  }

}