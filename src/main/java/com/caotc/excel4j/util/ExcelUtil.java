package com.caotc.excel4j.util;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.IntStream;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.parse.result.SheetParseResult;
import com.caotc.excel4j.parse.result.WorkbookParseResult;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;


public class ExcelUtil {
  // TODO 转移到Menu类中?
  // public static boolean isDataCell(Cell cell,Menu menu,Collection<Menu> menus){
  // if(cell==null){
  // return false;
  // }
  // CellRangeAddress cellRangeAddress=getMergedRegion(cell);
  // if(cellRangeAddress!=null){
  // return false;
  // }
  //
  // String cellString=getStringValue(cell);
  // if(StringUtils.isEmpty(cellString)){
  // CellStyle cellStyle=cell.getCellStyle();
  // short boderType=CellStyle.BORDER_NONE;
  // switch(menu.getCheckMenuConfig().getDirection()){
  // case TOP:boderType=cellStyle.getBorderTop();break;
  // case BOTTOM:boderType=cellStyle.getBorderBottom();break;
  // case LEFT:boderType=cellStyle.getBorderLeft();break;
  // default:boderType=cellStyle.getBorderRight();
  // }
  // if(boderType==CellStyle.BORDER_NONE){
  // return false;
  // }
  // }
  //
  // List<Cell> menuCells=Lists.newArrayList();
  // for(Menu m:menus){
  // menuCells.add(m.getCell());
  // }
  //
  // if(menuCells.contains(cell)){
  // return false;
  // }
  // return true;
  // }

  public static WorkbookParseResult parse(Workbook workbook, WorkbookConfig config) {
    return config.parse(workbook);
  }

  // TODO can?
  public static SheetParseResult parse(Sheet sheet, SheetConfig config) {
    return config.parse(sheet).build();
  }

  /**
   * 根据数据单元格数目并非固定的菜单数据检查配置对象集合将工作簿的的数据处理为一个Map的集合，一个map为一条数据
   * 
   * @author caotc
   * @date 2016.4.24
   * @param sheet 工作簿
   * @param menuDataCheckConfigList 数据单元格数目并非固定的菜单数据检查配置对象集合
   * @return 数据的集合
   */
  // TODO 改为直接解析后获取数据
  // public static List<Map<String,String>> getNoFixedDatas(Sheet sheet,Collection<SheetConfig>
  // menuDataCheckConfigList) {
  // List<SheetConfig> noFixedDataConfigs= new ArrayList<SheetConfig>();
  // for(SheetConfig menuDataCheckConfig:menuDataCheckConfigList){
  // if(menuDataCheckConfig.isDataFlag() && !menuDataCheckConfig.isSingleDataFlag()){
  // noFixedDataConfigs.add(menuDataCheckConfig);
  // }
  // }
  // List<Map<String,String>> list=new ArrayList<Map<String,String>>();
  // Map<SheetConfig,Cell> lastCellMap=new HashMap<SheetConfig,Cell>();
  // Map<String,String> map=new LinkedHashMap<String,String>();
  // for(SheetConfig menuDataCheckConfig:noFixedDataConfigs){
  // Cell menuCell=getCellByMenuName(sheet,
  // menuDataCheckConfig.getMenuNameMatcher().getMatchString());
  // Cell dataCell=menuDataCheckConfig.getDirection().nextCell(menuCell);
  // checkAndAddDataCell(dataCell,menuDataCheckConfig,menuDataCheckConfigList,lastCellMap,map);
  // }
  // while(!map.isEmpty()){
  // list.add(map);
  // map=new LinkedHashMap<String,String>();
  // for(SheetConfig menuDataCheckConfig:noFixedDataConfigs){
  // Cell lastDataCell=lastCellMap.get(menuDataCheckConfig);
  // Cell dataCell=menuDataCheckConfig.getDirection().nextCell(lastDataCell);
  // checkAndAddDataCell(dataCell,menuDataCheckConfig,menuDataCheckConfigList,lastCellMap,map);
  // }
  // }
  // return list;
  // }

  /**
   * 在工作簿中根据传入的数据单元格数目固定的菜单数据检查配置对象得到该菜单数据检查配置对象的数据单元格集合
   * 
   * @author caotc
   * @date 2016.4.24
   * @param sheet 工作簿
   * @param menuDataCheckConfig 数据单元格数目固定的菜单数据检查配置对象
   * @return 该菜单数据检查配置对象的数据单元格集合
   */
  // TODO 改为直接解析后获取数据
  // public static String getFixedDatas(Sheet sheet,SheetConfig menuDataCheckConfig) {
  // Cell menuCell=getCellByMenuName(sheet,
  // menuDataCheckConfig.getMenuNameMatcher().getMatchString());
  // Cell dataCell=menuDataCheckConfig.getDirection().nextCell(menuCell);
  // return getStringValue(dataCell);
  // }

  // TODO 解决报错
  // public static final JSONArray getDatas(Sheet sheet,SheetConfig sheetConfig){
  // JSONArray datas=new JSONArray();
  // List<Map<String,String>> noFixedDatas=getNoFixedDatas(sheet, menuDataCheckConfigs);
  // Map<String,String> fixedDatas=Maps.newHashMap();
  // for(SheetConfig menuDataCheckConfig:menuDataCheckConfigs){
  // if(menuDataCheckConfig.isDataFlag() && menuDataCheckConfig.isSingleDataFlag()){
  // String value=getFixedDatas(sheet,menuDataCheckConfig);
  // fixedDatas.put(menuDataCheckConfig.getMenuNameMatcher().getMatchString(), value);
  // }
  // }
  //
  // for(Map<String,String> noFixedData:noFixedDatas){
  // JSONObject json=new JSONObject();
  // json.putAll(noFixedData);
  // json.putAll(fixedDatas);
  // datas.add(json);
  // }
  // return datas;
  // }

  // TODO 解决报错
  // public static final <T> List<T> getDatas(Sheet sheet,SheetConfig sheetConfig, Class<T> clazz)
  // {
  // JSONArray datas=getDatas(sheet, menuDataCheckConfigs);
  // List<T> javaDatas=Lists.newArrayList();
  // for(int i=0;i<datas.size();i++){
  // javaDatas.add(JSONObject.toJavaObject(datas.getJSONObject(i), clazz));
  // }
  // return javaDatas;
  // }

  public static Cell getCellByIndex(Sheet sheet, int rowIndex, int columnIndex,
      MissingCellPolicy policy) {
    Row row = sheet.getRow(rowIndex);
    if (row == null) {
      row = sheet.createRow(rowIndex);
    }

    return row.getCell(columnIndex, policy);
  }

  public static Cell getCellByIndex(Sheet sheet, int rowIndex, int columnIndex) {
    return getCellByIndex(sheet, rowIndex, columnIndex, MissingCellPolicy.CREATE_NULL_AS_BLANK);
  }

  public static boolean isMergedRegion(Cell cell) {
    return getMergedRegion(cell) == null;
  }

  /**
   * 检查传入的单元格是否为合并单元格，是则返回该合并单元格对象
   * 
   * @author caotc
   * @date 2016.4.24
   * @param cell 单元格
   * @return 该合并单元格对象，不是则为null
   */
  public static CellRangeAddress getMergedRegion(Cell cell) {
    if (cell == null) {
      return null;
    }
    return getMergedRegion(cell.getSheet(), cell.getRowIndex(), cell.getColumnIndex());
  }

  /**
   * 判断指定的单元格是否是合并单元格，是则返回该合并单元格对象
   * 
   * @author caotc
   * @date 2016.4.24
   * @param sheet 工作簿
   * @param rowIndex 行下标
   * @param columnIndex 列下标
   * @return 该合并单元格对象，不是则为null
   */
  public static CellRangeAddress getMergedRegion(Sheet sheet, int rowIndex, int columnIndex) {
    if (sheet != null) {
      int sheetMergeCount = sheet.getNumMergedRegions();
      for (int i = 0; i < sheetMergeCount; i++) {
        CellRangeAddress range = sheet.getMergedRegion(i);
        if (range.isInRange(rowIndex, columnIndex)) {
          return range;
        }
      }
    }
    return null;
  }

  public static Collection<Cell> getCells(Sheet sheet, CellRangeAddress cellRangeAddress) {
    if (sheet == null || cellRangeAddress == null) {
      return Collections.emptyList();
    }
    List<Cell> cells = Lists.newLinkedList();
    for (int rowIndex = cellRangeAddress.getFirstRow(); rowIndex <= cellRangeAddress
        .getLastRow(); rowIndex++) {
      for (int columnIndex = cellRangeAddress.getFirstColumn(); columnIndex <= cellRangeAddress
          .getLastColumn(); columnIndex++) {
        cells.add(getCellByIndex(sheet, rowIndex, columnIndex));
      }
    }
    return cells;
  }

  /**
   * 判断传入的工作簿中是否含有合并单元格
   * 
   * @author caotc
   * @date 2016.4.24
   * @param sheet 工作簿
   * @return 是否含有合并单元格
   */
  public static boolean hasMerged(Sheet sheet) {
    return sheet.getNumMergedRegions() > 0;
  }

  public static void setDataFromEntity(Sheet sheet, List<?> datas, Map<String, SheetConfig> map,
      SimpleDateFormat sdf) {
    // if(datas!=null && !datas.isEmpty()){
    // for(Entry<String,SheetConfig> entry:map.entrySet()){
    // Field field=null;
    // try {
    // field=datas.get(0).getClass().getDeclaredField(entry.getKey());
    // if(field!=null){
    // field.setAccessible(true);
    // SheetConfig config=entry.getValue();
    // if(config.isDataFlag()){
    // if(config.isSingleDataFlag()){
    // Cell menuCell=getCellByMenuName(sheet, config.getMenuNameMatcher().getMatchString());
    // if(menuCell!=null){
    // Cell dataCell=config.getDirection().nextCell(menuCell);
    // if(dataCell!=null){
    // Object value=field.get(datas.get(0));
    // setCellValue(dataCell,value,sdf);
    // }
    // }
    // }else{
    // Cell menuCell=getCellByMenuName(sheet, config.getMenuNameMatcher().getMatchString());
    // if(menuCell!=null){
    // CellStyle cellStyle=config.getDirection().nextCell(menuCell).getCellStyle();
    // Cell dataCell=null;
    // for(int i=0;i<datas.size();i++){
    // dataCell=config.getDirection().nextCell(menuCell);
    // Object value=field.get(datas.get(i));
    // setCellValue(dataCell,value,sdf);
    // dataCell.setCellStyle(cellStyle);
    // }
    // }
    // }
    // }
    // field.setAccessible(false);
    // }
    // } catch (Exception e) {
    // e.printStackTrace();
    // }
    // }
    // }
  }

  public static void setDataFromMap(Sheet sheet, Collection<Map<String, Object>> datas,
      Collection<SheetConfig> menuConfigs, SimpleDateFormat sdf) {
    // if(datas!=null && !datas.isEmpty()){
    // ParseResult parseResult=parseMenu(sheet, menuConfigs);
    // for(Menu menu:parseResult.getFixedMenus()){
    // Cell dataCell=menu.getDataCell(1);
    // setCellValue(dataCell,datas.iterator().next().get(menu.getMenuConfig().getFieldName()),sdf);
    // }
    // int i=1;
    // for(Map<String,Object> data:datas){
    // for(Menu menu:parseResult.getNoFixedMenus()){
    // CellStyle cellStyle=menu.getDataCell(1).getCellStyle();
    // Cell dataCell=menu.getDataCell(i);
    // dataCell.setCellStyle(cellStyle);
    // setCellValue(dataCell,data.get(menu.getCheckMenuConfig().getFieldName()),sdf);
    // }
    // i++;
    // }
    // }
  }

  public static void setCellValue(Cell cell, Object value, SimpleDateFormat sdf) {
    if (value != null) {
      if (value instanceof Date && sdf != null) {
        cell.setCellValue(sdf.format(value));
      } else if (value instanceof Number) {
        cell.setCellValue(((Number) value).doubleValue());
      } else {
        cell.setCellValue(value.toString());
      }
    }
  }

  public static int getCellLeftColumnIndex(Cell cell) {
    CellRangeAddress cellRangeAddress = getMergedRegion(cell);
    int columnIndex =
        cellRangeAddress == null ? cell.getColumnIndex() : cellRangeAddress.getFirstColumn();
    return columnIndex - 1;
  }

  public static int getCellRightColumnIndex(Cell cell) {
    CellRangeAddress cellRangeAddress = getMergedRegion(cell);
    int columnIndex =
        cellRangeAddress == null ? cell.getColumnIndex() : cellRangeAddress.getLastColumn();
    return columnIndex + 1;
  }

  public static int getCellTopRowIndex(Cell cell) {
    CellRangeAddress cellRangeAddress = getMergedRegion(cell);
    int rowIndex = cellRangeAddress == null ? cell.getRowIndex() : cellRangeAddress.getFirstRow();
    return rowIndex - 1;
  }

  public static int getCellBottomRowIndex(Cell cell) {
    CellRangeAddress cellRangeAddress = getMergedRegion(cell);
    int rowIndex = cellRangeAddress == null ? cell.getRowIndex() : cellRangeAddress.getLastRow();
    return rowIndex + 1;
  }

  public static void moveCell(Collection<Cell> cells, int rowMoveNumber, int columnMoveNumber) {
    Set<CellRangeAddress> addresses = Sets.newHashSet();
    for (Cell cell : cells) {
      CellRangeAddress address = getMergedRegion(cell);
      if (address != null) {
        addresses.add(address);
      } else {
        moveCell(cell, rowMoveNumber, columnMoveNumber);
      }
    }
    Sheet sheet = cells.iterator().next().getSheet();
    for (CellRangeAddress address : addresses) {
      moveCell(sheet, address, rowMoveNumber, columnMoveNumber);
    }
  }

  public static void moveCell(Cell cell, int rowMoveNumber, int columnMoveNumber) {
    int rowIndex = cell.getRowIndex();
    int columnIndex = cell.getColumnIndex();
    Cell targetCell =
        getCellByIndex(cell.getSheet(), rowIndex + rowMoveNumber, columnIndex + columnMoveNumber);
    removeCell(targetCell);
    targetCell =
        getCellByIndex(cell.getSheet(), rowIndex + rowMoveNumber, columnIndex + columnMoveNumber);
    copyCell(cell, targetCell, true);
    removeCell(cell);
  }

  public static void moveCell(Sheet sheet, CellRangeAddress address, int rowMoveNumber,
      int columnMoveNumber) {
    int firstRow = address.getFirstRow();
    int lastRow = address.getLastRow();
    int firstColumn = address.getFirstColumn();
    int lastColumn = address.getLastColumn();
    for (int row = firstRow; row <= lastRow; row++) {
      for (int column = firstColumn; column <= lastColumn; column++) {
        Cell moveCell = getCellByIndex(sheet, row, column);
        Cell targetCell = getCellByIndex(sheet, row + rowMoveNumber, column + columnMoveNumber);
        removeCell(targetCell);
        targetCell = getCellByIndex(sheet, row + rowMoveNumber, column + columnMoveNumber);
        copyCell(moveCell, targetCell, true);
        removeCell(moveCell);
      }
    }

    CellRangeAddress targetAddress = new CellRangeAddress(firstRow + rowMoveNumber,
        lastRow + rowMoveNumber, firstColumn + columnMoveNumber, lastColumn + columnMoveNumber);
    sheet.addMergedRegion(targetAddress);
  }

  public static void removeCell(Cell cell) {
    CellRangeAddress address = getMergedRegion(cell);
    if (address != null) {
      removeCell(cell.getSheet(), address);
    } else {
      cell.getRow()
          .removeCell(getCellByIndex(cell.getSheet(), cell.getRowIndex(), cell.getColumnIndex()));
    }
  }

  public static void removeCell(Sheet sheet, CellRangeAddress address) {
    int sheetMergeCount = sheet.getNumMergedRegions();
    for (int i = sheetMergeCount - 1; i >= 0; i--) {
      CellRangeAddress range = sheet.getMergedRegion(i);
      if (range.getFirstRow() == address.getFirstRow() && range.getLastRow() == address.getLastRow()
          && range.getFirstColumn() == address.getFirstColumn()
          && range.getLastColumn() == address.getLastColumn()) {
        sheet.removeMergedRegion(i);
        for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
          for (int columnIndex = address.getFirstColumn(); columnIndex <= address
              .getLastColumn(); columnIndex++) {
            removeCell(getCellByIndex(sheet, rowIndex, columnIndex));
          }
        }
      }
    }
  }

  // TODO 改成children?或者直接删除?
  // public static List<String> getSubordinateMenus(Sheet sheet,String menuName){
  // List<String> menus=Lists.newArrayList();
  // Cell menu=getCellByMenuName(sheet,menuName);
  // if(menu!=null){
  // CellRangeAddress cellRangeAddress=getMergedRegion(menu);
  // if(cellRangeAddress==null){
  // return null;
  // }
  // int startColumn=cellRangeAddress.getFirstColumn();
  // int endColumn=cellRangeAddress.getLastColumn();
  // for(int column=startColumn;column<=endColumn;){
  // Cell subordinateMenu=getCellByIndex(sheet,getCellBottomRowIndex(menu),column);
  // menus.add(getStringValue(subordinateMenu));
  // column=getCellRightColumnIndex(subordinateMenu);
  // }
  // }
  // return menus;
  // }

  public static void setCellStyle(Cell cell, CellStyle style) {
    CellRangeAddress address = getMergedRegion(cell);
    if (address == null) {
      cell.setCellStyle(style);
    } else {
      for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
        for (int columnIndex = address.getFirstColumn(); columnIndex <= address
            .getLastColumn(); columnIndex++) {
          Cell c = getCellByIndex(cell.getSheet(), rowIndex, columnIndex);
          c.setCellStyle(style);
        }
      }
    }
  }

  public static void copyCell(Cell srcCell, Cell targetCell, boolean copyValueFlag) {
    // 样式
    targetCell.setCellStyle(srcCell.getCellStyle());
    // 评论
    if (srcCell.getCellComment() != null) {
      targetCell.setCellComment(srcCell.getCellComment());
    }
    // 不同数据类型处理
    CellType srcCellType = srcCell.getCellTypeEnum();
    targetCell.setCellType(srcCellType);
    if (copyValueFlag) {
      if (srcCellType == CellType.NUMERIC) {
        if (DateUtil.isCellDateFormatted(srcCell)) {
          targetCell.setCellValue(srcCell.getDateCellValue());
        } else {
          targetCell.setCellValue(srcCell.getNumericCellValue());
        }
      } else if (srcCellType == CellType.STRING) {
        targetCell.setCellValue(srcCell.getRichStringCellValue());
      } else if (srcCellType == CellType.BLANK) {
        // nothing21
      } else if (srcCellType == CellType.BOOLEAN) {
        targetCell.setCellValue(srcCell.getBooleanCellValue());
      } else if (srcCellType == CellType.ERROR) {
        targetCell.setCellErrorValue(srcCell.getErrorCellValue());
      } else if (srcCellType == CellType.FORMULA) {
        targetCell.setCellFormula(srcCell.getCellFormula());
      } else { // nothing29
      }
    }
  }

  public static boolean isDateCell(Cell cell) {
    return BaseDataType.DATE_TIME.test(getValue(cell));
  }

  public static Date getDate(Cell cell) {
    // TODO require not null? not a DateCell?
    if (Objects.isNull(cell) || CellType.BLANK.equals(cell.getCellTypeEnum())) {
      return null;
    }
    return BaseDataType.DATE_TIME.cast(getValue(cell), Date.class);
  }

  public static Cell getFirstCell(Sheet sheet, CellRangeAddress mergedRegion) {
    if (sheet == null || mergedRegion == null) {
      return null;
    }
    return getCellByIndex(sheet, mergedRegion.getFirstRow(), mergedRegion.getFirstColumn());
  }

  public static Cell getFirstCell(Cell cell) {
    if (isMergedRegion(cell)) {
      return getFirstCell(cell.getSheet(), getMergedRegion(cell));
    }
    return cell;
  }

  public static Object getValue(Cell cell) {
    // TODO require null
    if (Objects.isNull(cell)) {
      return null;
    }

    CellType cellType = cell.getCellTypeEnum();
    if (CellType.FORMULA.equals(cellType)) {
      cellType = cell.getCachedFormulaResultTypeEnum();
    }

    switch (cellType) {
      case NUMERIC:
        if (DateUtil.isCellDateFormatted(cell)) {
          return cell.getDateCellValue();
        } else {
          return cell.getNumericCellValue();
        }
      case STRING:
        return cell.getStringCellValue();
      case BOOLEAN:
        return cell.getBooleanCellValue();
      // TODO 由于getErrorCellValue()方法获得的是excel中的数字类型的错误码,似乎没有实际意义,暂定仍返回null
      case ERROR:
      default:
        throw new IllegalArgumentException(
            "the CellType " + cellType + " of " + cell + " is not support");
    }
  }

  /**
   * 获取单元格内的内容，并以字符串的形式返回
   * 
   * @author caotc
   * @date 2016.4.24
   * @param cell 单元格
   * @return 单元格内容的字符串
   */
  public static String getStringValue(Cell cell) {
    // TODO 根据返回类型不同选择不同DataType?
    return BaseDataType.STRING.cast(getValue(cell), String.class);
  }

  public static void removeMergedRegion(Sheet sheet, CellRangeAddress cellAddress) {
    IntStream.range(0, sheet.getNumMergedRegions());
    int sheetMergeCount = sheet.getNumMergedRegions();
    for (int i = 0; i < sheetMergeCount; i++) {
      CellRangeAddress range = sheet.getMergedRegion(i);
      int firstColumnIndex = range.getFirstColumn();
      int lastColumnIndex = range.getLastColumn();
      int firstRowIndex = range.getFirstRow();
      int lastRowIndex = range.getLastRow();
      if (cellAddress != null) {
        if ((cellAddress.getFirstColumn() == firstColumnIndex)
            && cellAddress.getLastColumn() == lastColumnIndex
            && cellAddress.getFirstRow() == firstRowIndex
            && cellAddress.getLastRow() == lastRowIndex) {
          sheet.removeMergedRegion(i);
        }
      }
    }
  }
}
