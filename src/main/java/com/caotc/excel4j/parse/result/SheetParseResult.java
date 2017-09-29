package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.parse.error.SheetError;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class SheetParseResult {
  private Sheet sheet;
  private SheetConfig sheetConfig;
  private List<SheetError> sheetErrors;

  public SheetParseResult(Sheet sheet, SheetConfig sheetConfig) {
    this.sheet = sheet;
    this.sheetConfig = sheetConfig;
  }

  public boolean hasError() {
    return !sheetErrors.isEmpty();
  }

  public SheetError getFirstError() {
    return sheetErrors.get(0);
  }

  public boolean addError(SheetError sheetError) {
    if (!sheetErrors.contains(sheetError)) {
      return sheetErrors.add(sheetError);
    }
    return Boolean.FALSE;
  }

  public SheetError addError(String errorMessage) {
    SheetError error = new SheetError(errorMessage);
    addError(error);
    return error;
  }

  public List<SheetError> getErrors() {
    return sheetErrors;
  }

  public void setErrors(List<SheetError> errors) {
    this.sheetErrors = errors;
  }

  public Sheet getSheet() {
    return sheet;
  }

  public void setSheet(Sheet sheet) {
    this.sheet = sheet;
  }

}
