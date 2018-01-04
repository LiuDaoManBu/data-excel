package com.caotc.excel4j.parse.result;

import java.util.List;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.WorkbookError;
import com.google.common.collect.ImmutableList;

public class WorkbookParseResult {
  public static WorkbookParseResult parse(Workbook workbook, WorkbookConfig workbookConfig) {
    return new WorkbookParseResult(workbook, workbookConfig);
  }

  private final Workbook workbook;
  private final WorkbookConfig workbookConfig;
  private final ImmutableList<WorkbookError> workbookErrors;
  private final ImmutableList<SheetParseResult> sheetParseResults;

  public WorkbookParseResult(Workbook workbook, WorkbookConfig workbookConfig) {
    super();
    this.workbook = workbook;
    this.workbookConfig = workbookConfig;
    // TODO
    workbookErrors = null;
    sheetParseResults = null;
  }

  public Workbook getWorkbook() {
    return workbook;
  }

  public WorkbookConfig getWorkbookConfig() {
    return workbookConfig;
  }

  public List<WorkbookError> getWorkbookErrors() {
    return workbookErrors;
  }

  public List<SheetParseResult> getSheetParseResults() {
    return sheetParseResults;
  }
}
