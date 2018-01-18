package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.Error;
import com.caotc.excel4j.parse.error.WorkbookError;
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

  public static Builder builder() {
    return new Builder();
  }

  private final Workbook workbook;
  private final WorkbookConfig config;
  private final ImmutableList<Error<Workbook>> errors;
  private final ImmutableList<SheetParseResult> sheetParseResults;

  private WorkbookParseResult(Builder builder) {
    this.workbook = builder.workbook;
    this.config = builder.config;
    this.errors = Optional.ofNullable(config.getMatcher())
        .map(matcher -> matcher.match(workbook).orElse(null))
        .map(message -> new Error<Workbook>(workbook, message)).map(ImmutableList::of)
        .orElse(ImmutableList.of());
    this.sheetParseResults = builder.sheetParseResultBuilders.stream()
        .peek(sheetParseResultBuilder -> sheetParseResultBuilder.setWorkbookParseResult(this))
        .map(SheetParseResult.Builder::build).collect(ImmutableList.toImmutableList());
  }

  public Workbook getWorkbook() {
    return workbook;
  }

  public WorkbookConfig getConfig() {
    return config;
  }

  public ImmutableList<com.caotc.excel4j.parse.error.Error<Workbook>> getErrors() {
    return errors;
  }

  public ImmutableList<SheetParseResult> getSheetParseResults() {
    return sheetParseResults;
  }

  public ImmutableList<Error<Workbook>> getAllErrors() {
    return Streams.concat(errors.stream(),
        sheetParseResults.stream().map(SheetParseResult::getAllErrors).flatMap(Collection::stream)
            .map(error -> new Error<Workbook>(workbook, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }
}
