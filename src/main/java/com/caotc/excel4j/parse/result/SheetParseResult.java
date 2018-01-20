package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.parse.error.ValidationError;
import com.google.common.collect.ImmutableList;

public class SheetParseResult {
  public static class Builder {
    private WorkbookParseResult workbookParseResult;
    private Sheet sheet;
    private SheetConfig config;
    private List<Table.Builder> tableBuilders;

    public SheetParseResult build() {
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
  private final ImmutableList<ValidationError<Sheet>> errors;
  private final ImmutableList<Table> tables;

  public SheetParseResult(Builder builder) {
    this.workbookParseResult = builder.workbookParseResult;
    this.sheet = builder.sheet;
    this.config = builder.config;
    // TODO need?
    this.errors =
        Optional.of(sheet).filter(sheet -> sheet.getLastRowNum() <= sheet.getFirstRowNum())
            .map(sheet -> new ValidationError<Sheet>(sheet, "don't have any data"))
            .map(ImmutableList::of).orElse(ImmutableList.of());
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

  public ImmutableList<ValidationError<Sheet>> getErrors() {
    return errors;
  }

  public ImmutableList<Table> getTables() {
    return tables;
  }

  public WorkbookParseResult getWorkbookParseResult() {
    return workbookParseResult;
  }

  public ImmutableList<ValidationError<Sheet>> getAllErrors() {
    return Stream
        .concat(errors.stream(),
            tables.stream().map(Table::getAllErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<Sheet>(sheet, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public Table getById(Object id) {
    return tables.stream().filter(table -> Objects.equals(table.getConfig().getId(), id)).findAny()
        .orElse(null);
  }
}
