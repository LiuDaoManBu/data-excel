package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.parse.error.WorkbookError;
import com.caotc.excel4j.parse.result.WorkbookParseResult;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

public class WorkbookConfig {
  public static class Builder {
    private List<SheetConfig.Builder> sheetConfigBuilders;
    private Matcher.Builder<Workbook> matcherBuilder;
    private ParserConfig parserConfig;

    public WorkbookConfig build() {
      return new WorkbookConfig(this);
    }

    public Matcher.Builder<Workbook> getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder setMatcherBuilder(Matcher.Builder<Workbook> matcherBuilder) {
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

    public List<SheetConfig.Builder> getSheetConfigBuilders() {
      return sheetConfigBuilders;
    }

    public Builder setSheetConfigBuilders(List<SheetConfig.Builder> sheetConfigBuilders) {
      this.sheetConfigBuilders = sheetConfigBuilders;
      return this;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final ImmutableCollection<SheetConfig> sheetConfigs;
  private final Matcher<Workbook> matcher;
  private final ParserConfig parserConfig;

  private WorkbookConfig(Builder builder) {
    this.sheetConfigs = builder.sheetConfigBuilders.stream()
        .peek(sheetConfigBuilder -> sheetConfigBuilder.setWorkbookConfig(this))
        .map(SheetConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    this.matcher =
        Optional.ofNullable(builder.matcherBuilder).map(Matcher.Builder::build).orElse(null);
    this.parserConfig = builder.parserConfig;
  }

  public WorkbookParseResult parse(Workbook workbook) {
    return WorkbookParseResult.builder().setWorkbook(workbook).setConfig(this).build();
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
  }

  public ImmutableCollection<SheetConfig> getSheetConfigs() {
    return sheetConfigs;
  }

  public Matcher<Workbook> getMatcher() {
    return matcher;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
