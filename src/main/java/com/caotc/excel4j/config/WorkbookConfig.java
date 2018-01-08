package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import com.caotc.excel4j.matcher.usermodel.WorkbookMatcher;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class WorkbookConfig {
  public static class Builder {
    private List<SheetConfig.Builder> sheetConfigs;
    private WorkbookMatcher matcher;
    private ParserConfig parserConfig;

    public WorkbookConfig builder() {
      parserConfig = Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
      return new WorkbookConfig(this);
    }

    public List<SheetConfig.Builder> getSheetConfigs() {
      return sheetConfigs;
    }

    public Builder setSheetConfigs(List<SheetConfig.Builder> sheetConfigs) {
      this.sheetConfigs = sheetConfigs;
      return this;
    }

    public WorkbookMatcher getMatcher() {
      return matcher;
    }

    public Builder setMatcher(WorkbookMatcher matcher) {
      this.matcher = matcher;
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

  private final ImmutableCollection<SheetConfig> sheetConfigs;
  private final WorkbookMatcher matcher;
  private final ParserConfig parserConfig;

  private WorkbookConfig(Builder builder) {
    this.sheetConfigs = builder.sheetConfigs.stream().map(SheetConfig.Builder::builder)
        .collect(ImmutableSet.toImmutableSet());
    this.matcher = builder.matcher;
    this.parserConfig = builder.parserConfig;
  }
  
}
