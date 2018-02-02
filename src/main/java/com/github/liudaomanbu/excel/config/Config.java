package com.github.liudaomanbu.excel.config;

import java.util.List;
import java.util.Optional;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.collect.ImmutableList;

public class Config<T> {
  public static class Builder<T> {
    private Object id;
    private ParserConfig parserConfig;
    private List<Validator<T>> validators;
    
    public Object getId() {
      return id;
    }

    public Builder<T> setId(Object id) {
      this.id = id;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder<T> setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

    public List<Validator<T>> getValidators() {
      return validators;
    }

    public Builder<T> setValidators(List<Validator<T>> validators) {
      this.validators = validators;
      return this;
    }
    
  }

  private final Object id;
  private final ParserConfig parserConfig;
  private final ImmutableList<Validator<T>> validators;

  public Config(Builder<T> builder) {
    id = Optional.ofNullable(builder.id).orElse(this.toString());
    parserConfig=builder.parserConfig;
    validators=builder.validators.stream().collect(ImmutableList.toImmutableList());
  }

  public Object getId() {
    return id;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public ImmutableList<Validator<T>> getValidators() {
    return validators;
  }
}
