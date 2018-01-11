package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.matcher.usermodel.WorkbookMatcher;
import com.caotc.excel4j.parse.error.WorkbookError;
import com.caotc.excel4j.parse.result.WorkbookParseResult;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
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

  private static final String SHEET_CONFIG_NO_MATCH_MESSAGE = "";

  public static Builder builder() {
    return new Builder();
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

  public WorkbookParseResult parse(Workbook workbook) {
    WorkbookParseResult.Builder builder =
        WorkbookParseResult.builder().setWorkbook(workbook).setConfig(this);
    if (matcher.test(workbook)) {
      ImmutableList<Sheet> sheets = IntStream.range(0, workbook.getNumberOfSheets())
          .mapToObj(workbook::getSheetAt).collect(ImmutableList.toImmutableList());
      // TODO Error设计不合理?getMessage Nullable?
      builder.setErrors(sheetConfigs.stream()
          .filter(config -> sheets.stream().noneMatch(config.getMatcher()::test))
          .map(config -> new WorkbookError(workbook, config.getMatcher().getMessage(null)))
          .collect(ImmutableList.toImmutableList()));
      // TODO sheet被多个matcher匹配的情况?
      builder.setSheetParseResultBuilders(sheetConfigs.stream()
          .filter(config -> sheets.stream().anyMatch(config.getMatcher()::test))
          .map(config -> sheets.stream().filter(config.getMatcher()::test).map(config::parse))
          .flatMap(Function.identity()).collect(ImmutableList.toImmutableList()));
    } else {
      builder
          .setErrors(ImmutableList.of(new WorkbookError(workbook, matcher.getMessage(workbook))));
    }
    return builder.build();
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
