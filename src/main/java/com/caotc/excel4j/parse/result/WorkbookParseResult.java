package com.caotc.excel4j.parse.result;

import java.util.List;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.WorkbookError;
import com.google.common.collect.ImmutableList;

public class WorkbookParseResult {
  public static class Builder {
    private Workbook workbook;
    private WorkbookConfig config;
    private List<WorkbookError> errors;
    private List<SheetParseResult.Builder> sheetParseResultBuilders;

    public WorkbookParseResult build() {
      errors = Optional.ofNullable(errors).orElseGet(ImmutableList::of);
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

    public List<WorkbookError> getErrors() {
      return errors;
    }

    public Builder setErrors(List<WorkbookError> errors) {
      this.errors = errors;
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
  private final ImmutableList<WorkbookError> errors;
  private final ImmutableList<SheetParseResult> sheetParseResults;

  private WorkbookParseResult(Builder builder) {
    this.workbook = builder.workbook;
    this.config = builder.config;
    this.errors = builder.errors.stream().collect(ImmutableList.toImmutableList());
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

  public ImmutableList<WorkbookError> getErrors() {
    return errors;
  }

  public ImmutableList<SheetParseResult> getSheetParseResults() {
    return sheetParseResults;
  }

}
