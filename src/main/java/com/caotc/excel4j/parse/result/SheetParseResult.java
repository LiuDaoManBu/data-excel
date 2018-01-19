package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.parse.error.ConstraintViolation;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Streams;
import com.caotc.excel4j.parse.error.ConstraintViolation;

public class SheetParseResult {
  public static class Builder {
    private WorkbookParseResult workbookParseResult;
    private Sheet sheet;
    private SheetConfig config;
    private List<ConstraintViolation<Sheet>> errors;
    private List<Table.Builder> tableBuilders;

    public SheetParseResult build() {
      errors = Optional.ofNullable(errors).orElse(ImmutableList.of());
      return new SheetParseResult(this);
    }

    public WorkbookParseResult getWorkbookParseResult() {
      return workbookParseResult;
    }

    public Builder setWorkbookParseResult(WorkbookParseResult workbookParseResult) {
      this.workbookParseResult = workbookParseResult;
      return this;
    }

    public Sheet getSheet() {
      return sheet;
    }

    public Builder setSheet(Sheet sheet) {
      this.sheet = sheet;
      return this;
    }

    public SheetConfig getConfig() {
      return config;
    }

    public Builder setConfig(SheetConfig config) {
      this.config = config;
      return this;
    }

    public List<ConstraintViolation<Sheet>> getErrors() {
      return errors;
    }

    public Builder setErrors(List<ConstraintViolation<Sheet>> errors) {
      this.errors = errors;
      return this;
    }

    public List<Table.Builder> getTableBuilders() {
      return tableBuilders;
    }

    public Builder setTableBuilders(List<Table.Builder> tableBuilders) {
      this.tableBuilders = tableBuilders;
      return this;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final WorkbookParseResult workbookParseResult;
  private final Sheet sheet;
  private final SheetConfig config;
  private final ImmutableList<ConstraintViolation<Sheet>> errors;
  private final ImmutableList<Table> tables;

  public SheetParseResult(Builder builder) {
    this.workbookParseResult = builder.workbookParseResult;
    this.sheet = builder.sheet;
    this.config = builder.config;
    // TODO builder.errors?
    // this.errors = builder.errors.stream().collect(ImmutableList.toImmutableList());
    this.errors =
        Optional.of(sheet).filter(sheet -> sheet.getLastRowNum() <= sheet.getFirstRowNum())
            .map(sheet -> new ConstraintViolation<Sheet>(sheet, "don't have any data")).map(ImmutableList::of)
            .orElse(ImmutableList.of());
    this.tables =
        builder.tableBuilders.stream().peek(tableBuilder -> tableBuilder.setSheetParseResult(this))
            .map(Table.Builder::build).collect(ImmutableList.toImmutableList());
  }

  public Sheet getSheet() {
    return sheet;
  }

  public SheetConfig getConfig() {
    return config;
  }

  public ImmutableList<ConstraintViolation<Sheet>> getErrors() {
    return errors;
  }

  public ImmutableList<Table> getTables() {
    return tables;
  }

  public WorkbookParseResult getWorkbookParseResult() {
    return workbookParseResult;
  }

  public ImmutableList<ConstraintViolation<Sheet>> getAllErrors() {
    return Streams.concat(errors.stream(),
        tables.stream().map(Table::getAllErrors).flatMap(Collection::stream)
        .map(error -> new ConstraintViolation<Sheet>(sheet, error.getMessage())))
    .collect(ImmutableList.toImmutableList());
  }
}
