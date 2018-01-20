package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.parse.result.SheetParseResult;
import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class SheetConfig {
  public static class Builder {
    private List<TableConfig.Builder> tableConfigBuilders;
    private WorkbookConfig workbookConfig;
    private Matcher<Sheet> matcher;
    private ParserConfig parserConfig;

    public Builder() {
      tableConfigBuilders = Lists.newLinkedList();
    }
    
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


    public Matcher<Sheet> getMatcher() {
      return matcher;
    }

    public Builder setMatcher(Matcher<Sheet> matcher) {
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

  private static final Predicate<Sheet> DEFAULT_MATCHER = Predicates.alwaysTrue();

  public static Builder builder() {
    return new Builder();
  }

  private final ImmutableCollection<TableConfig> tableConfigs;
  private final WorkbookConfig workbookConfig;
  private final Predicate<Sheet> matcher;
  private final ParserConfig parserConfig;

  private SheetConfig(Builder builder) {
    this.tableConfigs = builder.tableConfigBuilders.stream()
        .peek(tableConfigBuilder -> tableConfigBuilder.setSheetConfig(this))
        .map(TableConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    this.workbookConfig = builder.workbookConfig;
    this.matcher =
        Optional.ofNullable(builder.matcher).map(Matcher::reduce).orElse(DEFAULT_MATCHER);
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

  public Predicate<Sheet> getMatcher() {
    return matcher;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }
}
