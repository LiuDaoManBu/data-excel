package com.caotc.excel4j.config;

import java.util.Optional;

public class Config {
  public static class Builder {
    private Object id;

    public Object getId() {
      return id;
    }

    public Builder setId(Object id) {
      this.id = id;
      return this;
    }

  }

  private final Object id;

  public Config(Builder builder) {
    super();
    //TODO
    this.id = Optional.ofNullable(builder.id).orElse(this.toString());
  }

  public Object getId() {
    return id;
  }

}
