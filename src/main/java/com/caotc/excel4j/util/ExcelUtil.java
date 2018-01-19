package com.caotc.excel4j.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;
import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.CellRangeAddress;
import com.caotc.excel4j.annotation.ExcelField;
import com.caotc.excel4j.annotation.ExcelSheet;
import com.caotc.excel4j.annotation.ExcelTable;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.MenuDataConfig;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.matcher.BaseValidator;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.usermodel.SheetMatcher;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.parse.result.WorkbookParseResult;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;


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

  public static SheetConfig.Builder toSheetConfig(Class<?> type) {
    return Optional.ofNullable(type).map(t -> t.getAnnotation(ExcelSheet.class)).map(t -> {
      SheetConfig.Builder builder = SheetConfig.builder();
      if (!Strings.isNullOrEmpty(t.name())) {
        builder.setMatcher(
            new SheetMatcher().add(StringMatcherType.EQUALS, t.name(), Sheet::getSheetName));
      }
      return builder;
    }).orElse(null);
  }

  public static <V> TableConfig.Builder toTableConfig(Class<V> type) {
    return Optional.ofNullable(type).map(t -> t.getAnnotation(ExcelTable.class)).map(t -> {
      return TableConfig.builder().setId(type).setTopMenuConfigBuilders(ClassUtil.getAllFields(type)
          .map(ExcelUtil::toConfig).filter(Objects::nonNull).collect(Collectors.toList()));
    }).orElse(null);
  }

  public static MenuConfig.Builder toConfig(Field field) {
    return Optional.ofNullable(field).map(f -> f.getAnnotation(ExcelField.class)).map(f -> {
      MenuConfig.Builder builder = MenuConfig.builder()
          .setMatcher(new StandardCellMatcher().addDataPredicate(StringMatcherType.EQUALS,
              f.menuName(), value -> BaseDataType.STRING.cast(value, String.class)))
          .setDirection(f.direction()).setDistance(f.distance()).setNecessity(f.necessity())
          .setDataConfigBuilder(MenuDataConfig.builder().setLoadType(f.loadType())
              .setDataType(f.dataType()).setField(field).setFieldName(field.getName()));
      Optional.ofNullable(field.getAnnotation(NotNull.class)).ifPresent(t -> {
        // TODO tip
        builder.getDataConfigBuilder().getValidators().add(0,
            new BaseValidator<>(Objects::nonNull, cell -> cell.formatAsString()));
      });
      return builder;
    }).orElse(null);
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

  // TODO 方法重写 指定cellType?
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

  // TODO 方法重写
  public static void moveCell(Collection<Cell> cells, int rowMoveNumber, int columnMoveNumber) {
    Set<CellRangeAddress> addresses = Sets.newHashSet();
    for (Cell cell : cells) {
      Optional<CellRangeAddress> address = getMergedRegion(cell);
      if (address.isPresent()) {
        addresses.add(address.get());
      } else {
        moveCell(cell, rowMoveNumber, columnMoveNumber);
      }
    }
    Sheet sheet = cells.iterator().next().getSheet();
    for (CellRangeAddress address : addresses) {
      moveCell(sheet, address, rowMoveNumber, columnMoveNumber);
    }
  }

  // TODO 方法重构
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

  // TODO 方法重构
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

  // TODO 方法重构
  public static void removeCell(@Nullable Cell cell) {
    Optional<CellRangeAddress> address = getMergedRegion(cell);
    if (address.isPresent()) {
      removeCell(cell.getSheet(), address.get());
    } else {
      cell.getRow()
          .removeCell(getCellByIndex(cell.getSheet(), cell.getRowIndex(), cell.getColumnIndex()));
    }
  }

  // TODO 方法重构
  public static void removeCell(@Nullable Sheet sheet, @Nullable CellRangeAddress address) {
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

  // TODO 方法重构 定义为StandardCell方法?
  public static void setCellStyle(Cell cell, CellStyle style) {
    Optional<CellRangeAddress> optional = getMergedRegion(cell);
    if (optional.isPresent()) {
      CellRangeAddress address = optional.get();
      for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
        for (int columnIndex = address.getFirstColumn(); columnIndex <= address
            .getLastColumn(); columnIndex++) {
          Cell c = getCellByIndex(cell.getSheet(), rowIndex, columnIndex);
          c.setCellStyle(style);
        }
      }
    } else {
      cell.setCellStyle(style);
    }
  }

  // TODO 方法重构
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

  public static boolean isDateCell(@Nullable Cell cell) {
    return Optional.ofNullable(cell).map(ExcelUtil::getValue).map(BaseDataType.DATE_TIME::test)
        .orElse(false);
  }

  @Nullable
  public static Date getDate(@Nullable Cell cell) {
    // TODO not a DateCell throw?
    return Optional.ofNullable(cell).map(ExcelUtil::getValue)
        .map(value -> BaseDataType.DATE_TIME.cast(value, Date.class)).orElse(null);
  }

  @Nullable
  public static Object getValue(@Nullable Cell cell) {
    return Optional.ofNullable(cell).map(Cell::getCellTypeEnum)
        .map(type -> CellType.FORMULA.equals(type) ? cell.getCachedFormulaResultTypeEnum() : type)
        .map(CELL_TYPE_TO_VALUE_FUNCTIONS::get).map(function -> function.apply(cell)).orElse(null);
  }

  public static String getStringValue(@Nullable Cell cell) {
    // TODO 根据返回类型不同选择不同DataType?
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
    return Optional.ofNullable(workbook).map(t -> IntStream.range(0, t.getNumberOfSheets()))
        .orElse(IntStream.empty()).mapToObj(workbook::getSheetAt);
  }

  public static Stream<Row> getRows(@Nullable Sheet sheet) {
    return getRows(sheet, null, null);
  }

  public static Stream<Row> getRows(@Nullable Sheet sheet, @Nullable Integer firstRowIndex,
      @Nullable Integer lastRowIndex) {
    // TODO sheet.getTopRow()? closed
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
