package com.github.liudaomanbu.excel.parse.result;

import com.github.liudaomanbu.excel.config.Config;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.collect.ImmutableList;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;

public class ParseResult<T> {
  public static class Builder<T> {
    private Config<T> config;
    private T model;

    public Config<T> getConfig() {
      return config;
    }

    public void setConfig(Config<T> config) {
      this.config = config;
    }

    public T getModel() {
      return model;
    }

    public void setModel(T model) {
      this.model = model;
    }
  }

  private final Config<T> config;
  private final T model;
  private final ImmutableList<ValidationError<T>> errors;

  protected ParseResult(Builder<T> builder) {
    config = builder.config;
    model = builder.model;
    errors = getValidators().filter(validator -> validator.needValidate(model))
        .map(validator -> validator.validate(model)).flatMap(Collection::stream)
        .collect(ImmutableList.toImmutableList());
  }

  public Stream<Validator<T>> getValidators() {
    return Optional.ofNullable(config).map(Config::getValidators).map(Collection::stream)
        .orElseGet(Stream::empty);
  }

  public Config<T> getConfig() {
    return config;
  }

  public T getModel() {
    return model;
  }

  public ImmutableList<ValidationError<T>> getErrors() {
    return errors;
  }
}
