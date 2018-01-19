package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.ValidationError;
import com.caotc.excel4j.parse.error.WorkbookError;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Streams;

public class WorkbookParseResult {
  public static class Builder {
    private Workbook workbook;
    private WorkbookConfig config;
    private List<SheetParseResult.Builder> sheetParseResultBuilders;

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

    public List<SheetParseResult.Builder> getSheetParseResultBuilders() {
      return sheetParseResultBuilders;
    }

    public Builder setSheetParseResultBuilders(
        List<SheetParseResult.Builder> sheetParseResultBuilders) {
      this.sheetParseResultBuilders = sheetParseResultBuilders;
      return this;
    }

  }

  private static final Function<SheetConfig, String> SHEET_CONFIG_NO_MATCH_MESSAGE_FUNCTION =
      config -> config + "don't have any matches sheet";

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
    // TODO sheet被多个matcher匹配的情况?
    builder.setSheetParseResultBuilders(config.getSheetConfigs().stream()
        .filter(config -> ExcelUtil.getSheets(workbook).anyMatch(config.getMatcher()::test))
        .flatMap(config -> ExcelUtil.getSheets(workbook).filter(config.getMatcher()::test)
            .map(config::parse))
        .collect(ImmutableList.toImmutableList()));

    sheetParseResults = builder.sheetParseResultBuilders.stream()
        .peek(sheetParseResultBuilder -> sheetParseResultBuilder.setWorkbookParseResult(this))
        .map(SheetParseResult.Builder::build).collect(ImmutableList.toImmutableList());      
    errors = config.getValidators().stream().map(validator->validator.validate(workbook))
        .flatMap(Collection::stream).collect(ImmutableList.toImmutableList());
  }

  public Workbook getWorkbook() {
    return workbook;
  }

  public WorkbookConfig getConfig() {
    return config;
  }

  public ImmutableList<com.caotc.excel4j.parse.error.ValidationError<Workbook>> getErrors() {
    return errors;
  }

  public ImmutableList<SheetParseResult> getSheetParseResults() {
    return sheetParseResults;
  }

  public ImmutableList<ValidationError<Workbook>> getAllErrors() {
    return Streams.concat(errors.stream(),
        sheetParseResults.stream().map(SheetParseResult::getAllErrors).flatMap(Collection::stream)
            .map(error -> new ValidationError<Workbook>(workbook, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }
}
