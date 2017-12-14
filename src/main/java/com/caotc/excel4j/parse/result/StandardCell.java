package com.caotc.excel4j.parse.result;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.EnumSet;
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
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSet.Builder;
import com.google.common.collect.Iterables;

public class StandardCell extends CellRangeAddress implements Cell {
  private static final int ONE = 1;

  public static StandardCell valueOf(Sheet sheet, CellRangeAddress cellRangeAddress) {
    if (sheet == null || cellRangeAddress == null) {
      return null;
    }
    return new StandardCell(sheet, cellRangeAddress);
  }

  public static StandardCell valueOf(Cell cell) {
    if (cell == null) {
      return null;
    }

    if (ExcelUtil.isMergedRegion(cell)) {
      return valueOf(cell.getSheet(), ExcelUtil.getMergedRegion(cell));
    }
    return new StandardCell(cell);
  }

  private final Sheet sheet;
  private final Cell valueCell;
  private final Collection<Cell> cells;

  private StandardCell(Sheet sheet, CellRangeAddress cellRangeAddress) {
    super(cellRangeAddress.getFirstRow(), cellRangeAddress.getLastRow(),
        cellRangeAddress.getFirstColumn(), cellRangeAddress.getLastColumn());
    Preconditions.checkNotNull(sheet);
    this.sheet = sheet;
    // TODO 是否需要出于安全性考虑复写一份私有方法在本类?
    this.valueCell = ExcelUtil.getFirstCell(sheet, this);
    this.cells = ExcelUtil.getCells(sheet, this);
  }

  private StandardCell(Cell cell) {
    super(cell.getRowIndex(), cell.getRowIndex(), cell.getColumnIndex(), cell.getColumnIndex());
    this.sheet = cell.getSheet();
    this.valueCell = cell;
    // TODO 是否需要出于安全性考虑复写一份私有方法在本类?
    this.cells = ExcelUtil.getCells(sheet, this);
  }

  public Object getValue() {
    return ExcelUtil.getValue(valueCell);
  }

  public boolean isMergedRegion() {
    return getNumberOfCells() > ONE;
  }

  public ImmutableCollection<Row> getRows(){
    Builder<Row> builder=ImmutableSet.builder();
    for(int rowIndex=getFirstRow();rowIndex<=getLastRow();rowIndex++) {
      builder.add(sheet.getRow(rowIndex));
    }
    return builder.build();
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
    return Iterables.any(EnumSet.allOf(Direction.class), this::isBorderCell);
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

  public Collection<Cell> getCells() {
    return cells;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((cells == null) ? 0 : cells.hashCode());
    result = prime * result + ((sheet == null) ? 0 : sheet.hashCode());
    result = prime * result + ((valueCell == null) ? 0 : valueCell.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    StandardCell other = (StandardCell) obj;
    if (cells == null) {
      if (other.cells != null)
        return false;
    } else if (!cells.equals(other.cells))
      return false;
    if (sheet == null) {
      if (other.sheet != null)
        return false;
    } else if (!sheet.equals(other.sheet))
      return false;
    if (valueCell == null) {
      if (other.valueCell != null)
        return false;
    } else if (!valueCell.equals(other.valueCell))
      return false;
    return true;
  }
}
