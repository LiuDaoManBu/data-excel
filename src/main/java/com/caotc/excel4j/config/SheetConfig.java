package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.matcher.usermodel.SheetMatcher;
import com.caotc.excel4j.parse.result.SheetParseResult;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

public class SheetConfig {
  public static class Builder {
    private List<TableConfig.Builder> tableConfigBuilders;
    private WorkbookConfig workbookConfig;
    private Matcher.Builder<Sheet> matcherBuilder;
    private ParserConfig parserConfig;

    public SheetConfig build() {
      return new SheetConfig(this);
    }

    public List<TableConfig.Builder> getTableConfigBuilders() {
      return tableConfigBuilders;
    }

    public Builder setTableConfigBuilders(List<TableConfig.Builder> tableConfigBuilders) {
      this.tableConfigBuilders = tableConfigBuilders;
      return this;
    }

    public WorkbookConfig getWorkbookConfig() {
      return workbookConfig;
    }

    public Builder setWorkbookConfig(WorkbookConfig workbookConfig) {
      this.workbookConfig = workbookConfig;
      return this;
    }

    public Matcher.Builder<Sheet> getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder setMatcherBuilder(Matcher.Builder<Sheet> matcherBuilder) {
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

  public static Builder builder() {
    return new Builder();
  }

  private final ImmutableCollection<TableConfig> tableConfigs;
  private final WorkbookConfig workbookConfig;
  private final Matcher<Sheet> matcher;
  private final ParserConfig parserConfig;

  private SheetConfig(Builder builder) {
    this.tableConfigs = builder.tableConfigBuilders.stream()
        .peek(tableConfigBuilder -> tableConfigBuilder.setSheetConfig(this))
        .map(TableConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    this.workbookConfig = builder.workbookConfig;
    this.matcher = builder.matcherBuilder.build();
    this.parserConfig = builder.parserConfig;
  }

  public SheetParseResult.Builder parse(Sheet sheet) {
    SheetParseResult.Builder builder = SheetParseResult.builder().setSheet(sheet).setConfig(this);
    builder.setTableBuilders(tableConfigs.stream().map(config -> config.parse(sheet))
        .collect(ImmutableList.toImmutableList()));
    // TODO sheetMatcher是BookErro,TableMatcher是TableError,Sheet没有error?
    // builder.setErrors();
    return builder;
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(workbookConfig.getEffectiveParserConfig());
  }
  
  public ImmutableCollection<TableConfig> getTableConfigs() {
    return tableConfigs;
  }

  public WorkbookConfig getWorkbookConfig() {
    return workbookConfig;
  }

  public Matcher<Sheet> getMatcher() {
    return matcher;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }
}
