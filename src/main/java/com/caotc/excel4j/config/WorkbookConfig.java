package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.parse.result.WorkbookParseResult;
import com.caotc.excel4j.util.ExcelUtil;
import com.caotc.excel4j.validator.BaseValidator;
import com.caotc.excel4j.validator.Validator;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class WorkbookConfig {
  public static class Builder {
    private List<SheetConfig.Builder> sheetConfigBuilders;
    private List<Validator<Workbook>> validators;
    private ParserConfig parserConfig;

    public Builder() {
      sheetConfigBuilders = Lists.newLinkedList();
      validators = Lists.newLinkedList();
    }

    public WorkbookConfig build() {
      return new WorkbookConfig(this);
    }

    public List<Validator<Workbook>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<Workbook>> validators) {
      this.validators = validators;
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
  private final ImmutableList<Validator<Workbook>> validators;
  private final ParserConfig parserConfig;

  private WorkbookConfig(Builder builder) {
    sheetConfigs = builder.sheetConfigBuilders.stream()
        .peek(sheetConfigBuilder -> sheetConfigBuilder.setWorkbookConfig(this))
        .map(SheetConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    // TODO 注释,重复匹配?tip,sheet可选是否必须匹配
    Validator<Workbook> validator = new BaseValidator<>(
        sheetConfigs.stream().collect(ImmutableMap.toImmutableMap(sheetConfig -> {
          Predicate<Workbook> predicate = workbook -> ExcelUtil.getSheets(workbook)
              .filter(sheetConfig.getMatcher()::test).findAny().isPresent();
          return predicate;
        }, sheetConfig -> {
          Function<Workbook, String> function = workbook -> sheetConfig + "没有匹配到任何结果";
          return function;
        })));
    validators = Stream.concat(Stream.of(validator), builder.validators.stream())
        .collect(ImmutableList.toImmutableList());
    parserConfig = builder.parserConfig;
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

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public ImmutableList<Validator<Workbook>> getValidators() {
    return validators;
  }

}
