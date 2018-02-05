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
    Preconditions.checkNotNull(sheet);
    Preconditions.checkNotNull(cellRangeAddress);
    return new StandardCell(sheet, cellRangeAddress);
  }

  public static StandardCell valueOf(Cell cell) {
    Preconditions.checkNotNull(cell);
    return ExcelUtil.findMergedRegion(cell)
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
  @Override
  public int getColumnIndex() {
    return valueCell.getColumnIndex();
  }

  @Override
  public int getRowIndex() {
    return valueCell.getRowIndex();
  }

  @Override
  public Row getRow() {
    return valueCell.getRow();
  }

  @Override
  @SuppressWarnings("deprecation")
  @Deprecated
  @Removal(version = "1.0")
  public void setCellType(int cellType) {
    cells.forEach(cell -> cell.setCellType(cellType));
  }

  @Override
  public void setCellType(CellType cellType) {
    cells.forEach(cell -> cell.setCellType(cellType));
  }

  @Override
  @Deprecated
  @Removal(version = "1.0")
  public int getCellType() {
    return valueCell.getCellType();
  }

  @Override
  public CellType getCellTypeEnum() {
    return valueCell.getCellTypeEnum();
  }

  @Override
  @Deprecated
  @Removal(version = "1.0")
  public int getCachedFormulaResultType() {
    return valueCell.getCachedFormulaResultType();
  }

  @Override
  public CellType getCachedFormulaResultTypeEnum() {
    return valueCell.getCachedFormulaResultTypeEnum();
  }

  @Override
  public void setCellValue(double value) {
    valueCell.setCellValue(value);
  }

  @Override
  public void setCellValue(Date value) {
    valueCell.setCellValue(value);
  }

  @Override
  public void setCellValue(Calendar value) {
    valueCell.setCellValue(value);
  }

  @Override
  public void setCellValue(RichTextString value) {
    valueCell.setCellValue(value);
  }

  @Override
  public void setCellValue(String value) {
    valueCell.setCellValue(value);
  }

  @Override
  public void setCellFormula(String formula) throws FormulaParseException {
    valueCell.setCellFormula(formula);
  }

  @Override
  public String getCellFormula() {
    return valueCell.getCellFormula();
  }

  @Override
  public double getNumericCellValue() {
    return valueCell.getNumericCellValue();
  }

  @Override
  public Date getDateCellValue() {
    return valueCell.getDateCellValue();
  }

  @Override
  public RichTextString getRichStringCellValue() {
    return valueCell.getRichStringCellValue();
  }

  @Override
  public String getStringCellValue() {
    return valueCell.getStringCellValue();
  }

  @Override
  public void setCellValue(boolean value) {
    valueCell.setCellValue(value);
  }

  @Override
  public void setCellErrorValue(byte value) {
    valueCell.setCellErrorValue(value);
  }

  @Override
  public boolean getBooleanCellValue() {
    return valueCell.getBooleanCellValue();
  }

  @Override
  public byte getErrorCellValue() {
    return valueCell.getErrorCellValue();
  }

  @Override
  public void setCellStyle(CellStyle style) {
    cells.forEach(cell -> cell.setCellStyle(style));
  }

  @Override
  public CellStyle getCellStyle() {
    return valueCell.getCellStyle();
  }

  @Override
  public void setAsActiveCell() {
    valueCell.setAsActiveCell();
  }

  @Override
  public CellAddress getAddress() {
    return valueCell.getAddress();
  }

  @Override
  public void setCellComment(Comment comment) {
    valueCell.setCellComment(comment);
  }

  @Override
  public Comment getCellComment() {
    return valueCell.getCellComment();
  }

  @Override
  public void removeCellComment() {
    valueCell.removeCellComment();
  }

  @Override
  public Hyperlink getHyperlink() {
    return valueCell.getHyperlink();
  }

  @Override
  public void setHyperlink(Hyperlink link) {
    valueCell.setHyperlink(link);
  }

  @Override
  public void removeHyperlink() {
    valueCell.removeHyperlink();
  }

  @Override
  public CellRangeAddress getArrayFormulaRange() {
    return valueCell.getArrayFormulaRange();
  }

  @Override
  public boolean isPartOfArrayFormulaGroup() {
    return valueCell.isPartOfArrayFormulaGroup();
  }
  // delegate Cell methods end

  @Override
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
