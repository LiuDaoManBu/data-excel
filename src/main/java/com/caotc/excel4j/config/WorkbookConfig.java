package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.matcher.usermodel.WorkbookMatcher;
import com.caotc.excel4j.parse.error.WorkbookError;
import com.caotc.excel4j.parse.result.WorkbookParseResult;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

public class WorkbookConfig {
  public static class Builder {
    private List<SheetConfig.Builder> sheetConfigBuilders;
    private WorkbookMatcher.Builder matcherBuilder;
    private ParserConfig parserConfig;

    public WorkbookConfig builder() {
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

  private static final Function<SheetConfig, String> SHEET_CONFIG_NO_MATCH_MESSAGE_FUNCTION =
      config -> config + "don't have any matches sheet";

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
    ImmutableList.Builder<WorkbookError> errors = ImmutableList.builder();
    Optional<WorkbookError> optional =
        matcher.match(workbook).map(message -> new WorkbookError(workbook, message));
    optional.ifPresent(errors::add);
    if (!optional.isPresent()) {
      ImmutableList<Sheet> sheets = ExcelUtil.getSheets(workbook);
      // TODO sheetConfig匹配不到假如matcher中直接返回所有error?
      sheetConfigs.stream().filter(config -> sheets.stream().noneMatch(config.getMatcher()::test))
          .map(config -> new WorkbookError(workbook,
              SHEET_CONFIG_NO_MATCH_MESSAGE_FUNCTION.apply(config)))
          .forEach(errors::add);


      // TODO sheet被多个matcher匹配的情况?
      builder.setSheetParseResultBuilders(sheetConfigs.stream()
          .filter(config -> sheets.stream().anyMatch(config.getMatcher()::test))
          .map(config -> sheets.stream().filter(config.getMatcher()::test).map(config::parse))
          .flatMap(Function.identity()).collect(ImmutableList.toImmutableList()));
    }
    builder.setErrors(errors.build());
    return builder.build();
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
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
