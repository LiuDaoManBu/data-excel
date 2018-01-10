package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import com.caotc.excel4j.matcher.usermodel.WorkbookMatcher;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class WorkbookConfig {
  public static class Builder {
    private List<SheetConfig.Builder> sheetConfigBuilders;
    private WorkbookMatcher.Builder matcherBuilder;
    private ParserConfig parserConfig;

    public WorkbookConfig builder() {
      parserConfig = Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
      return new WorkbookConfig(this);
    }

    public WorkbookMatcher.Builder getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder setMatcherBuilder(WorkbookMatcher.Builder matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
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
    this.sheetConfigs = builder.sheetConfigBuilders.stream()
        .peek(sheetConfigBuilder -> sheetConfigBuilder.setWorkbookConfig(this))
        .map(SheetConfig.Builder::builder).collect(ImmutableSet.toImmutableSet());
    this.matcher = builder.matcherBuilder.build();
    this.parserConfig = builder.parserConfig;
  }

  public ImmutableCollection<SheetConfig> getSheetConfigs() {
    return sheetConfigs;
  }

  public WorkbookMatcher getMatcher() {
    return matcher;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
