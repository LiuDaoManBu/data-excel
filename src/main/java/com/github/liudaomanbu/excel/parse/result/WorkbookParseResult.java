package com.github.liudaomanbu.excel.parse.result;

import com.github.liudaomanbu.excel.validator.Validators;
import java.util.Collection;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Workbook;
import com.github.liudaomanbu.excel.config.WorkbookConfig;
import com.github.liudaomanbu.excel.constant.Necessity;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.github.liudaomanbu.excel.validator.BaseValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

public class WorkbookParseResult{
  public static class Builder{
    private Workbook workbook;
    private WorkbookConfig config;

    public WorkbookParseResult build() {
      return new WorkbookParseResult(this);
    }

    public Workbook getWorkbook() {
      return workbook;
    }

    public Builder setWorkbook(Workbook workbook) {
      this.workbook = workbook;
      return this;
    }

    public WorkbookConfig getConfig() {
      return config;
    }

    public Builder setConfig(WorkbookConfig config) {
      this.config = config;
      return this;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final Workbook workbook;
  private final WorkbookConfig config;
  private final ImmutableList<ValidationError<Workbook>> errors;
  private final ImmutableList<SheetParseResult> sheetParseResults;

  private WorkbookParseResult(Builder builder) {
    workbook = builder.workbook;
    config = builder.config;
    sheetParseResults = createSheetParseResultBuilders().stream()
        .peek(sheetParseResultBuilder -> sheetParseResultBuilder.setWorkbookParseResult(this))
        .map(SheetParseResult.Builder::build).collect(ImmutableList.toImmutableList());
    errors = getValidators().filter(validator -> validator.needValidate(workbook))
        .map(validator -> validator.validate(workbook)).flatMap(Collection::stream)
        .collect(ImmutableList.toImmutableList());
  }

  private ImmutableList<SheetParseResult.Builder> createSheetParseResultBuilders() {
    return config.getSheetConfigs().stream()
        .map(sheetConfig -> ExcelUtil.getSheets(workbook).filter(sheetConfig.getMatcher()::test)
            .findAny().map(sheetConfig::parse))
        .filter(Optional::isPresent).map(Optional::get).collect(ImmutableList.toImmutableList());
  }

  private Stream<Validator<Workbook>> getValidators() {
    return Stream.concat(config.getValidators().stream(), Stream.of(createSheetConfigValidator()));
  }

  private Validator<Workbook> createSheetConfigValidator() {
    return Validators.create(
        config.getSheetConfigs().stream().filter(t -> Necessity.MUST.equals(t.getNecessity()))
            .collect(ImmutableMap.toImmutableMap(sheetConfig -> {
              Predicate<Workbook> predicate = workbook -> ExcelUtil.getSheets(workbook)
                  .filter(sheetConfig.getMatcher()::test).findAny().isPresent();
              return predicate;
            }, sheetConfig -> {
              Function<Workbook, String> function =
                  workbook -> "没有匹配到" + sheetConfig.getId() + "对应的工作簿";
              return function;
            })));
  }

  public ImmutableList<ValidationError<Workbook>> getAllErrors() {
    return Stream.concat(errors.stream(),
        sheetParseResults.stream().map(SheetParseResult::getAllErrors).flatMap(Collection::stream)
            .map(error -> new ValidationError<Workbook>(workbook, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public boolean hasError() {
    return !getAllErrors().isEmpty();
  }

  public Workbook getWorkbook() {
    return workbook;
  }

  public WorkbookConfig getConfig() {
    return config;
  }

  public ImmutableList<ValidationError<Workbook>> getErrors() {
    return errors;
  }

  public ImmutableList<SheetParseResult> getSheetParseResults() {
    return sheetParseResults;
  }
}
