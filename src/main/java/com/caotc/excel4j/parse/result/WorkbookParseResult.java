package com.caotc.excel4j.parse.result;

import java.util.List;
import org.apache.poi.ss.usermodel.Workbook;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.error.WorkbookError;

public class WorkbookParseResult {
  public static WorkbookParseResult parse() {
    return new WorkbookParseResult();
  }
  
  private Workbook workbook;
  private WorkbookConfig workbookConfig;
  private List<WorkbookError> workbookErrors;
  private List<SheetParseResult> sheetParseResults;

}
