package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.ConstraintViolation;
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
  private final ImmutableList<ConstraintViolation<Workbook>> errors;
  private final ImmutableList<SheetParseResult> sheetParseResults;

  private WorkbookParseResult(Builder builder) {
    this.workbook = builder.workbook;
    this.config = builder.config;
    ImmutableList.Builder<ConstraintViolation<Workbook>> errors = ImmutableList.builder();
    Optional<ConstraintViolation<Workbook>> optional =
        Optional.ofNullable(config.getMatcher()).map(m -> m.match(workbook).orElse(null))
            .map(message -> new ConstraintViolation<Workbook>(workbook, message));
    optional.ifPresent(errors::add);
    if (!optional.isPresent()) {
      // TODO sheetConfig匹配不到假如matcher中直接返回所有error?
      config.getSheetConfigs().stream()
          .filter(config -> ExcelUtil.getSheets(workbook).noneMatch(config.getMatcher()::test))
          .map(config -> new ConstraintViolation<Workbook>(workbook,
              SHEET_CONFIG_NO_MATCH_MESSAGE_FUNCTION.apply(config)))
          .forEach(errors::add);


      // TODO sheet被多个matcher匹配的情况?
      builder.setSheetParseResultBuilders(config.getSheetConfigs().stream()
          .filter(config -> ExcelUtil.getSheets(workbook).anyMatch(config.getMatcher()::test))
          .flatMap(config -> ExcelUtil.getSheets(workbook).filter(config.getMatcher()::test)
              .map(config::parse))
          .collect(ImmutableList.toImmutableList()));

      this.sheetParseResults = builder.sheetParseResultBuilders.stream()
          .peek(sheetParseResultBuilder -> sheetParseResultBuilder.setWorkbookParseResult(this))
          .map(SheetParseResult.Builder::build).collect(ImmutableList.toImmutableList());
    } else {
      this.sheetParseResults = ImmutableList.of();
    }
    this.errors = errors.build();
  }

  public Workbook getWorkbook() {
    return workbook;
  }

  public WorkbookConfig getConfig() {
    return config;
  }

  public ImmutableList<com.caotc.excel4j.parse.error.ConstraintViolation<Workbook>> getErrors() {
    return errors;
  }

  public ImmutableList<SheetParseResult> getSheetParseResults() {
    return sheetParseResults;
  }

  public ImmutableList<ConstraintViolation<Workbook>> getAllErrors() {
    return Streams.concat(errors.stream(),
        sheetParseResults.stream().map(SheetParseResult::getAllErrors).flatMap(Collection::stream)
            .map(error -> new ConstraintViolation<Workbook>(workbook, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }
}
