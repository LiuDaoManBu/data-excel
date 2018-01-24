package com.github.liudaomanbu.excel.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.annotation.Nullable;
import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.CellRangeAddress;
import com.alibaba.fastjson.JSONObject;
import com.github.liudaomanbu.excel.annotation.ExcelField;
import com.github.liudaomanbu.excel.annotation.ExcelMenu;
import com.github.liudaomanbu.excel.annotation.ExcelSheet;
import com.github.liudaomanbu.excel.annotation.ExcelTable;
import com.github.liudaomanbu.excel.config.MenuConfig;
import com.github.liudaomanbu.excel.config.MenuDataConfig;
import com.github.liudaomanbu.excel.config.SheetConfig;
import com.github.liudaomanbu.excel.config.TableConfig;
import com.github.liudaomanbu.excel.config.TableDataConfig;
import com.github.liudaomanbu.excel.config.WorkbookConfig;
import com.github.liudaomanbu.excel.matcher.constant.StringMatcherType;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.matcher.usermodel.SheetMatcher;
import com.github.liudaomanbu.excel.matcher.usermodel.StandardCellMatcher;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.StandardCell;
import com.github.liudaomanbu.excel.parse.result.WorkbookParseResult;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Maps;
import com.google.common.collect.Streams;


public class ExcelUtil {
  private static final ImmutableMap<CellType, Function<Cell, Object>> CELL_TYPE_TO_VALUE_FUNCTIONS =
      ImmutableMap.<CellType, Function<Cell, Object>>builder()
          .put(CellType.NUMERIC,
              cell -> DateUtil.isCellDateFormatted(cell) ? cell.getDateCellValue()
                  : cell.getNumericCellValue())
          .put(CellType.STRING, Cell::getStringCellValue)
          .put(CellType.BOOLEAN, Cell::getBooleanCellValue).build();

  public static final MissingCellPolicy DEFAULT_MISSING_CELL_POLICY =
      MissingCellPolicy.RETURN_NULL_AND_BLANK;

  public static WorkbookParseResult parse(File file, WorkbookConfig config)
      throws EncryptedDocumentException, InvalidFormatException, IOException {
    return config.parse(WorkbookFactory.create(file));
  }

  public static WorkbookParseResult parse(InputStream inputStream, WorkbookConfig config)
      throws EncryptedDocumentException, InvalidFormatException, IOException {
    return config.parse(WorkbookFactory.create(inputStream));
  }

  public static SheetConfig.Builder parseToSheetConfig(Class<?> type) {
    return Optional.ofNullable(type).map(t -> t.getAnnotation(ExcelSheet.class)).map(t -> {
      SheetConfig.Builder builder = SheetConfig.builder();
      if (!Strings.isNullOrEmpty(t.value())) {
        builder.setId(t.value()).setMatcher(
            new SheetMatcher().add(t.valueMatcherType(), t.value(), Sheet::getSheetName));
      }
      return builder;
    }).orElse(null);
  }

  public static <T> TableConfig.Builder<T> parseToTableConfig(Class<T> type) {
    return Optional.ofNullable(type).map(t -> t.getAnnotation(ExcelTable.class)).map(t -> {
      List<MenuConfig.Builder<T>> topMenuConfigBuilders =
          ClassUtil.getAllFields(type).map(f -> ExcelUtil.<T>parseToMenuConfig(f))
              .filter(Objects::nonNull).collect(Collectors.toList());
      
//      Multimap<Object, Object> idTochildrenIds=HashMultimap.create();
//      ClassUtil.getAllFields(type).forEach(field->{
//        ExcelMenu[] menus=field.getAnnotation(ExcelField.class).menus();
//        if(menus.length==0) {
//          idTochildrenIds.put(field.getName(), null);
//        }
//        if(menus.length==1) {
//          idTochildrenIds.put(menus[0].value(), null);
//        }
//        for(int i=0;i<menus.length-1;i++) {
//          idTochildrenIds.put(menus[i], menus[i+1]);
//        }
//      });
      
//      topMenuConfigBuilders.stream().map(MenuConfig.Builder::get)
          

      TableConfig.Builder<T> builder =
          TableConfig.<T>builder().setId(type).setTopMenuConfigBuilders(topMenuConfigBuilders)
          .setDataConfigBuilder(TableDataConfig.<T>builder().setType(type));
      return builder;
    }).orElse(null);
  }

  public static <T> T toJavaObject(Map<Menu<T>, StandardCell> menuToValueCell, Class<T> type) {
    return new JSONObject(toJsonObject(menuToValueCell)).toJavaObject(type);
  }

  public static <T> Map<String, Object> toJsonObject(Map<Menu<T>, StandardCell> menuToValueCell) {
    Map<String, Object> jsonObject = Maps.newHashMap();
    menuToValueCell.forEach((menu, cell) -> {
      Field field = menu.getField();
      Object value = null;
      if (Objects.isNull(field)) {
        value = cell.getValue();
      } else {
        value = menu.getData().getConfig().getDataType().cast(cell.getValue(), field.getType());
      }

      jsonObject.put(menu.getFieldName(), value);
    });
    return jsonObject;
  }

  public static <T> MenuConfig.Builder<T> parseToMenuConfig(Field field) {
    return Optional.ofNullable(field).map(f -> f.getAnnotation(ExcelField.class)).map(f -> {
      ImmutableList<MenuConfig.Builder<T>> builders=Arrays.stream(f.menus()).map(menu -> ExcelUtil.<T>parseToMenuConfig(menu))
          .collect(ImmutableList.toImmutableList());
      
      MenuConfig.Builder<T> dataMenuBuilder =builders.stream().reduce((first, second) -> {
        first.getChildrenBuilders().add(second);
        return second;
      }).orElse(MenuConfig.<T>builder().setId(field.getName())
          .setMatcher(new StandardCellMatcher().addDataPredicate(StringMatcherType.EQUALS,
              field.getName(), value -> BaseDataType.STRING.cast(value, String.class))))
      .setDataConfigBuilder(parseToMenuDataConfig(field));
      
      return builders.stream().findFirst().orElse(dataMenuBuilder);
    }).orElse(null);
  }

  private static <T> MenuConfig.Builder<T> parseToMenuConfig(ExcelMenu excelMenu) {
    return MenuConfig.<T>builder().setId(excelMenu.value())
        .setMatcher(new StandardCellMatcher().addDataPredicate(excelMenu.valueMatcherType(),
            excelMenu.value(), value -> BaseDataType.STRING.cast(value, String.class)))
        .setDirection(excelMenu.direction()).setDistance(excelMenu.distance())
        .setNecessity(excelMenu.necessity());
  }

  public static <T> MenuDataConfig.Builder<T> parseToMenuDataConfig(Field field) {
    return Optional.ofNullable(field).map(f -> f.getAnnotation(ExcelField.class)).map(f -> {
      return MenuDataConfig.<T>builder().setLoadType(f.loadType())
          .setDataType(findDataType(f.dataType(), field)).setField(field)
          .setFieldName(field.getName());
    }).orElse(null);
  }

  private static BaseDataType findDataType(BaseDataType dataType, Field field) {
    if(!BaseDataType.NATURAL.equals(dataType)) {
      return dataType;
    }
    if (field.getType().equals(String.class)) {
      return BaseDataType.STRING;
    }
    if (field.getType().isEnum()) {
      return BaseDataType.ENUM;
    }
    ImmutableCollection<Class<?>> wholeNumbers =
        ImmutableSet.of(byte.class, Byte.class, short.class, Short.class, int.class, Integer.class,
            long.class, Long.class, BigInteger.class);
    if (wholeNumbers.contains(field.getType())) {
      return BaseDataType.WHOLE_NUMBER;
    }

    ImmutableCollection<Class<?>> decimals =
        ImmutableSet.of(float.class, Float.class, double.class, Double.class, BigDecimal.class);
    if (decimals.contains(field.getType())) {
      return BaseDataType.DECIMAL;
    }

    if (boolean.class.equals(field.getType()) || Boolean.class.equals(field.getType())) {
      return BaseDataType.BOOLEAN;
    }

    if (Date.class.equals(field.getType()) || LocalDateTime.class.equals(field.getType())) {
      return BaseDataType.DATE_TIME;
    }

    if (LocalDate.class.equals(field.getType())) {
      return BaseDataType.DATE;
    }

    if (LocalTime.class.equals(field.getType())) {
      return BaseDataType.TIME;
    }

    throw new IllegalArgumentException();
  }

  @Nullable
  public static Cell getCellByIndex(Sheet sheet, int rowIndex, int columnIndex) {
    return getCell(sheet, rowIndex, columnIndex, DEFAULT_MISSING_CELL_POLICY);
  }

  @Nullable
  public static Cell getCell(@Nullable Sheet sheet, int rowIndex, int columnIndex,
      @Nullable MissingCellPolicy policy) {
    return Optional.ofNullable(sheet).map(t -> t.getRow(rowIndex)).map(row -> row
        .getCell(columnIndex, Optional.ofNullable(policy).orElse(DEFAULT_MISSING_CELL_POLICY)))
        .orElse(null);
  }

  public static boolean isMergedRegion(@Nullable Cell cell) {
    return getMergedRegion(cell).isPresent();
  }

  public static Optional<CellRangeAddress> getMergedRegion(@Nullable Cell cell) {
    return Optional.ofNullable(cell)
        .map(t -> getMergedRegion(t.getSheet(), t.getRowIndex(), t.getColumnIndex())).get();
  }

  public static Optional<CellRangeAddress> getMergedRegion(@Nullable Sheet sheet, int rowIndex,
      int columnIndex) {
    return getMergedRegions(sheet).filter(address -> address.isInRange(rowIndex, columnIndex))
        .findAny();
  }

  public static Optional<StandardCell> toStandardCell(@Nullable Cell cell) {
    return Optional.ofNullable(cell).map(StandardCell::valueOf);
  }

  public static boolean hasMergedRegion(@Nullable Sheet sheet) {
    return Optional.ofNullable(sheet).map(Sheet::getNumMergedRegions).map(n -> n > 0).orElse(false);
  }

  // 指定cellType?
  public static void setCellValue(@Nullable Cell cell, @Nullable Object value) {
    Optional.ofNullable(cell).ifPresent(t -> {
      if (Objects.isNull(value)) {
        cell.setCellType(CellType.BLANK);
      } else {
        if (value instanceof Boolean) {
          cell.setCellValue((Boolean) value);
        }
        if (value instanceof Date) {
          cell.setCellValue((Date) value);
        }
        if (value instanceof Calendar) {
          cell.setCellValue((Calendar) value);
        }
        if (value instanceof Double) {
          cell.setCellValue((Double) value);
        }
        cell.setCellValue(value.toString());
      }
    });
  }

  // public static void moveCell(Collection<Cell> cells, int rowMoveNumber, int columnMoveNumber) {
  // Set<CellRangeAddress> addresses = Sets.newHashSet();
  // for (Cell cell : cells) {
  // Optional<CellRangeAddress> address = getMergedRegion(cell);
  // if (address.isPresent()) {
  // addresses.add(address.get());
  // } else {
  // moveCell(cell, rowMoveNumber, columnMoveNumber);
  // }
  // }
  // Sheet sheet = cells.iterator().next().getSheet();
  // for (CellRangeAddress address : addresses) {
  // moveCell(sheet, address, rowMoveNumber, columnMoveNumber);
  // }
  // }

  // public static void moveCell(Cell cell, int rowMoveNumber, int columnMoveNumber) {
  // int rowIndex = cell.getRowIndex();
  // int columnIndex = cell.getColumnIndex();
  // Cell targetCell =
  // getCellByIndex(cell.getSheet(), rowIndex + rowMoveNumber, columnIndex + columnMoveNumber);
  // removeCell(targetCell);
  // targetCell =
  // getCellByIndex(cell.getSheet(), rowIndex + rowMoveNumber, columnIndex + columnMoveNumber);
  // copyCell(cell, targetCell, true);
  // removeCell(cell);
  // }

  // public static void moveCell(Sheet sheet, CellRangeAddress address, int rowMoveNumber,
  // int columnMoveNumber) {
  // int firstRow = address.getFirstRow();
  // int lastRow = address.getLastRow();
  // int firstColumn = address.getFirstColumn();
  // int lastColumn = address.getLastColumn();
  // for (int row = firstRow; row <= lastRow; row++) {
  // for (int column = firstColumn; column <= lastColumn; column++) {
  // Cell moveCell = getCellByIndex(sheet, row, column);
  // Cell targetCell = getCellByIndex(sheet, row + rowMoveNumber, column + columnMoveNumber);
  // removeCell(targetCell);
  // targetCell = getCellByIndex(sheet, row + rowMoveNumber, column + columnMoveNumber);
  // copyCell(moveCell, targetCell, true);
  // removeCell(moveCell);
  // }
  // }
  //
  // CellRangeAddress targetAddress = new CellRangeAddress(firstRow + rowMoveNumber,
  // lastRow + rowMoveNumber, firstColumn + columnMoveNumber, lastColumn + columnMoveNumber);
  // sheet.addMergedRegion(targetAddress);
  // }

  // public static void removeCell(@Nullable Cell cell) {
  // Optional<CellRangeAddress> address = getMergedRegion(cell);
  // if (address.isPresent()) {
  // removeCell(cell.getSheet(), address.get());
  // } else {
  // cell.getRow()
  // .removeCell(getCellByIndex(cell.getSheet(), cell.getRowIndex(), cell.getColumnIndex()));
  // }
  // }

  // public static void removeCell(@Nullable Sheet sheet, @Nullable CellRangeAddress address) {
  // int sheetMergeCount = sheet.getNumMergedRegions();
  // for (int i = sheetMergeCount - 1; i >= 0; i--) {
  // CellRangeAddress range = sheet.getMergedRegion(i);
  // if (range.getFirstRow() == address.getFirstRow() && range.getLastRow() == address.getLastRow()
  // && range.getFirstColumn() == address.getFirstColumn()
  // && range.getLastColumn() == address.getLastColumn()) {
  // sheet.removeMergedRegion(i);
  // for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
  // for (int columnIndex = address.getFirstColumn(); columnIndex <= address
  // .getLastColumn(); columnIndex++) {
  // removeCell(getCellByIndex(sheet, rowIndex, columnIndex));
  // }
  // }
  // }
  // }
  // }

  // public static void setCellStyle(Cell cell, CellStyle style) {
  // Optional<CellRangeAddress> optional = getMergedRegion(cell);
  // if (optional.isPresent()) {
  // CellRangeAddress address = optional.get();
  // for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
  // for (int columnIndex = address.getFirstColumn(); columnIndex <= address
  // .getLastColumn(); columnIndex++) {
  // Cell c = getCellByIndex(cell.getSheet(), rowIndex, columnIndex);
  // c.setCellStyle(style);
  // }
  // }
  // } else {
  // cell.setCellStyle(style);
  // }
  // }

  // public static void copyCell(Cell srcCell, Cell targetCell, boolean copyValueFlag) {
  // // 样式
  // targetCell.setCellStyle(srcCell.getCellStyle());
  // // 评论
  // if (srcCell.getCellComment() != null) {
  // targetCell.setCellComment(srcCell.getCellComment());
  // }
  // // 不同数据类型处理
  // CellType srcCellType = srcCell.getCellTypeEnum();
  // targetCell.setCellType(srcCellType);
  // if (copyValueFlag) {
  // if (srcCellType == CellType.NUMERIC) {
  // if (DateUtil.isCellDateFormatted(srcCell)) {
  // targetCell.setCellValue(srcCell.getDateCellValue());
  // } else {
  // targetCell.setCellValue(srcCell.getNumericCellValue());
  // }
  // } else if (srcCellType == CellType.STRING) {
  // targetCell.setCellValue(srcCell.getRichStringCellValue());
  // } else if (srcCellType == CellType.BLANK) {
  // // nothing21
  // } else if (srcCellType == CellType.BOOLEAN) {
  // targetCell.setCellValue(srcCell.getBooleanCellValue());
  // } else if (srcCellType == CellType.ERROR) {
  // targetCell.setCellErrorValue(srcCell.getErrorCellValue());
  // } else if (srcCellType == CellType.FORMULA) {
  // targetCell.setCellFormula(srcCell.getCellFormula());
  // } else { // nothing29
  // }
  // }
  // }

  @Nullable
  public static Object getValue(@Nullable Cell cell) {
    return Optional.ofNullable(cell).map(Cell::getCellTypeEnum)
        .map(type -> CellType.FORMULA.equals(type) ? cell.getCachedFormulaResultTypeEnum() : type)
        .map(CELL_TYPE_TO_VALUE_FUNCTIONS::get).map(function -> function.apply(cell)).orElse(null);
  }

  public static String getStringValue(@Nullable Cell cell) {
    return Optional.ofNullable(cell).map(ExcelUtil::getValue)
        .map(value -> BaseDataType.STRING.cast(value, String.class)).orElse(null);
  }

  public static void removeMergedRegion(@Nullable Sheet sheet,
      @Nullable CellRangeAddress cellAddress) {
    if (Objects.nonNull(sheet) && Objects.nonNull(cellAddress)) {
      AtomicInteger index = new AtomicInteger();
      getMergedRegions(sheet)
          .collect(ImmutableMap.toImmutableMap(Function.identity(), t -> index.incrementAndGet()))
          .entrySet().stream().filter(entry -> cellAddress.equals(entry.getKey())).findAny()
          .ifPresent(entry -> sheet.removeMergedRegion(entry.getValue()));
    }
  }

  public static Stream<Sheet> getSheets(@Nullable Workbook workbook) {
    return Optional.ofNullable(workbook).map(Streams::stream).orElse(Stream.empty());
  }

  public static Stream<Row> getRows(@Nullable Sheet sheet) {
    return Optional.ofNullable(sheet).map(Streams::stream).orElse(Stream.empty());
  }

  public static Stream<Row> getRows(@Nullable Sheet sheet, @Nullable Integer firstRowIndex,
      @Nullable Integer lastRowIndex) {
    return Optional.ofNullable(sheet)
        .map(t -> IntStream.range(Optional.ofNullable(firstRowIndex).orElse(t.getFirstRowNum()),
            Optional.ofNullable(lastRowIndex).orElse(t.getLastRowNum())))
        .orElse(IntStream.empty()).mapToObj(sheet::getRow);
  }

  public static Stream<CellRangeAddress> getMergedRegions(@Nullable Sheet sheet) {
    return Optional.ofNullable(sheet).map(t -> IntStream.range(0, t.getNumMergedRegions()))
        .orElse(IntStream.empty()).mapToObj(sheet::getMergedRegion);
  }

  public static Stream<Cell> getCells(@Nullable Sheet sheet) {
    return getCells(sheet, DEFAULT_MISSING_CELL_POLICY);
  }

  public static Stream<Cell> getCells(@Nullable Sheet sheet, @Nullable MissingCellPolicy policy) {
    MissingCellPolicy effectivePolicy =
        Optional.ofNullable(policy).orElse(DEFAULT_MISSING_CELL_POLICY);
    return getRows(sheet).flatMap(row -> getCells(row, effectivePolicy));
  }

  public static Stream<Cell> getCells(@Nullable Sheet sheet,
      @Nullable CellRangeAddress cellRangeAddress) {
    return getCells(sheet, cellRangeAddress, DEFAULT_MISSING_CELL_POLICY);
  }

  public static Stream<Cell> getCells(@Nullable Sheet sheet,
      @Nullable CellRangeAddress cellRangeAddress, @Nullable MissingCellPolicy policy) {
    if (Objects.isNull(sheet) || Objects.isNull(cellRangeAddress)) {
      return Stream.empty();
    }

    MissingCellPolicy effectivePolicy =
        Optional.ofNullable(policy).orElse(DEFAULT_MISSING_CELL_POLICY);
    return getRows(sheet, cellRangeAddress.getFirstRow(), cellRangeAddress.getLastRow())
        .flatMap(row -> getCells(row, cellRangeAddress.getFirstColumn(),
            cellRangeAddress.getLastColumn(), effectivePolicy));
  }

  public static Stream<Cell> getCells(@Nullable Row row) {
    return getCells(row, DEFAULT_MISSING_CELL_POLICY);
  }

  public static Stream<Cell> getCells(@Nullable Row row, @Nullable MissingCellPolicy policy) {
    return getCells(row, null, null, policy);
  }

  public static Stream<Cell> getCells(@Nullable Row row, @Nullable Integer firstColumnIndex,
      @Nullable Integer lastColumnIndex) {
    return getCells(row, firstColumnIndex, lastColumnIndex, DEFAULT_MISSING_CELL_POLICY);
  }

  public static Stream<Cell> getCells(@Nullable Row row, @Nullable Integer firstColumnIndex,
      @Nullable Integer lastColumnIndex, @Nullable MissingCellPolicy policy) {
    MissingCellPolicy effectivePolicy =
        Optional.ofNullable(policy).orElse(DEFAULT_MISSING_CELL_POLICY);
    return Optional.ofNullable(row)
        .map(t -> IntStream.range(
            Optional.ofNullable(firstColumnIndex).orElse((int) t.getFirstCellNum()),
            Optional.ofNullable(lastColumnIndex).orElse((int) t.getLastCellNum())))
        .orElse(IntStream.empty()).mapToObj(i -> row.getCell(i, effectivePolicy));
  }
}
