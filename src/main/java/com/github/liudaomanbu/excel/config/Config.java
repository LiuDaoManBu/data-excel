package com.github.liudaomanbu.excel.config;

import java.util.Optional;

public class Config {
  public static class Builder {
    private Object id;
    private ParserConfig parserConfig;

    public Object getId() {
      return id;
    }

    public Builder setId(Object id) {
      this.id = id;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }
    
  }

  private final Object id;
  private final ParserConfig parserConfig;

  public Config(Builder builder) {
    super();
    id = Optional.ofNullable(builder.id).orElse(this.toString());
    parserConfig=builder.parserConfig;
  }

  public Object getId() {
    return id;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
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
