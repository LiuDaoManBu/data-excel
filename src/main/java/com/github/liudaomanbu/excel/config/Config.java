package com.github.liudaomanbu.excel.config;

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
    this.id = Optional.ofNullable(builder.id).orElse(this.toString());
  }

  public Object getId() {
    return id;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    Config other = (Config) obj;
    if (id == null) {
      if (other.id != null)
        return false;
    } else if (!id.equals(other.id))
      return false;
    return true;
  }

  @Override
  public String toString() {
    return "Config [id=" + id + "]";
  }

}
