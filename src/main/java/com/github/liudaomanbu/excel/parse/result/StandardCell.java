package com.github.liudaomanbu.excel.parse.result;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import org.apache.poi.ss.formula.FormulaParseException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Comment;
import org.apache.poi.ss.usermodel.Hyperlink;
import org.apache.poi.ss.usermodel.RichTextString;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.util.Removal;
import com.github.liudaomanbu.excel.constant.Direction;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class StandardCell extends CellRangeAddress implements Cell {
  private static final int ONE = 1;

  public static StandardCell valueOf(Sheet sheet, CellRangeAddress cellRangeAddress) {
    // TODO tip
    Preconditions.checkNotNull(sheet);
    Preconditions.checkNotNull(cellRangeAddress);
    return new StandardCell(sheet, cellRangeAddress);
  }

  public static StandardCell valueOf(Cell cell) {
    // TODO tip
    Preconditions.checkNotNull(cell);
    return ExcelUtil.getMergedRegion(cell)
        .map(address -> new StandardCell(cell.getSheet(), address))
        .orElse(new StandardCell(cell.getSheet(), new CellRangeAddress(cell.getRowIndex(),
            cell.getRowIndex(), cell.getColumnIndex(), cell.getColumnIndex())));
  }

  private final Sheet sheet;
  private final Cell valueCell;
  private final ImmutableCollection<Cell> cells;

  private StandardCell(Sheet sheet, CellRangeAddress cellRangeAddress) {
    super(cellRangeAddress.getFirstRow(), cellRangeAddress.getLastRow(),
        cellRangeAddress.getFirstColumn(), cellRangeAddress.getLastColumn());

    this.sheet = sheet;
    // TODO 是否需要出于安全性考虑复写一份私有方法在本类?
    this.valueCell = ExcelUtil.getCellByIndex(sheet, getFirstRow(), getFirstColumn());
    this.cells = ExcelUtil.getCells(sheet, this).collect(ImmutableSet.toImmutableSet());
  }

  public Object getValue() {
    return ExcelUtil.getValue(valueCell);
  }

  public boolean isMergedRegion() {
    return getNumberOfCells() > ONE;
  }

  public boolean isTopBorderCell() {
    return isBorderCell(Direction.TOP);
  }

  public boolean isBottomBorderCell() {
    return isBorderCell(Direction.BOTTOM);
  }

  public boolean isLeftBorderCell() {
    return isBorderCell(Direction.LEFT);
  }

  public boolean isRightBorderCell() {
    return isBorderCell(Direction.RIGHT);
  }

  public boolean isBorderCell(Direction direction) {
    return direction.isBorderCell(this);
  }

  public boolean isBorderCell() {
    return Arrays.stream(Direction.values()).anyMatch(this::isBorderCell);
  }

  // delegate Cell methods start
  // TODO 是否需要为了防止误用而抛错?
  public int getColumnIndex() {
    return valueCell.getColumnIndex();
  }

  // TODO 是否需要为了防止误用而抛错?
  public int getRowIndex() {
    return valueCell.getRowIndex();
  }

  // TODO 是否需要为了防止误用而抛错?
  public Row getRow() {
    return valueCell.getRow();
  }

  @SuppressWarnings("deprecation")
  @Deprecated
  @Removal(version = "1.0")
  public void setCellType(int cellType) {
    cells.forEach(cell -> cell.setCellType(cellType));
  }

  public void setCellType(CellType cellType) {
    cells.forEach(cell -> cell.setCellType(cellType));
  }

  @Deprecated
  @Removal(version = "1.0")
  public int getCellType() {
    return valueCell.getCellType();
  }

  public CellType getCellTypeEnum() {
    return valueCell.getCellTypeEnum();
  }

  @Deprecated
  @Removal(version = "1.0")
  public int getCachedFormulaResultType() {
    return valueCell.getCachedFormulaResultType();
  }

  public CellType getCachedFormulaResultTypeEnum() {
    return valueCell.getCachedFormulaResultTypeEnum();
  }

  public void setCellValue(double value) {
    valueCell.setCellValue(value);
  }

  public void setCellValue(Date value) {
    valueCell.setCellValue(value);
  }

  public void setCellValue(Calendar value) {
    valueCell.setCellValue(value);
  }

  public void setCellValue(RichTextString value) {
    valueCell.setCellValue(value);
  }

  public void setCellValue(String value) {
    valueCell.setCellValue(value);
  }

  public void setCellFormula(String formula) throws FormulaParseException {
    valueCell.setCellFormula(formula);
  }

  public String getCellFormula() {
    return valueCell.getCellFormula();
  }

  public double getNumericCellValue() {
    return valueCell.getNumericCellValue();
  }

  public Date getDateCellValue() {
    return valueCell.getDateCellValue();
  }

  public RichTextString getRichStringCellValue() {
    return valueCell.getRichStringCellValue();
  }

  public String getStringCellValue() {
    return valueCell.getStringCellValue();
  }

  public void setCellValue(boolean value) {
    valueCell.setCellValue(value);
  }

  public void setCellErrorValue(byte value) {
    valueCell.setCellErrorValue(value);
  }

  public boolean getBooleanCellValue() {
    return valueCell.getBooleanCellValue();
  }

  public byte getErrorCellValue() {
    return valueCell.getErrorCellValue();
  }

  public void setCellStyle(CellStyle style) {
    cells.forEach(cell -> cell.setCellStyle(style));
  }

  public CellStyle getCellStyle() {
    return valueCell.getCellStyle();
  }

  public void setAsActiveCell() {
    // TODO 为合并单元格时是否会选中整个合并单元格?
    valueCell.setAsActiveCell();
  }

  public CellAddress getAddress() {
    return valueCell.getAddress();
  }

  public void setCellComment(Comment comment) {
    // TODO 为合并单元格时是否需要为每个单元格都分配注释?
    valueCell.setCellComment(comment);
  }

  public Comment getCellComment() {
    return valueCell.getCellComment();
  }

  public void removeCellComment() {
    // TODO 为合并单元格时是否需要为每个单元格都删除注释?
    valueCell.removeCellComment();
  }

  public Hyperlink getHyperlink() {
    return valueCell.getHyperlink();
  }

  public void setHyperlink(Hyperlink link) {
    // TODO 为合并单元格时是否需要为每个单元格都分配超链接?
    valueCell.setHyperlink(link);
  }

  public void removeHyperlink() {
    // TODO 为合并单元格时是否需要为每个单元格都删除超链接?
    valueCell.removeHyperlink();
  }

  public CellRangeAddress getArrayFormulaRange() {
    return valueCell.getArrayFormulaRange();
  }

  public boolean isPartOfArrayFormulaGroup() {
    return valueCell.isPartOfArrayFormulaGroup();
  }
  // delegate Cell methods end

  public Sheet getSheet() {
    return sheet;
  }

  public Cell getValueCell() {
    return valueCell;
  }

  public ImmutableCollection<Cell> getCells() {
    return cells;
  }
}
