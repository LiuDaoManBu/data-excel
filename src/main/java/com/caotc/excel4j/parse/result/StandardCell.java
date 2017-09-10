package com.caotc.excel4j.parse.result;

import org.apache.poi.ss.SpreadsheetVersion;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressBase;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.util.LittleEndianOutput;

import com.caotc.excel4j.util.ExcelUtil;

public class StandardCell{
	private static final int ONE=1;
	
	public static final StandardCell valueOf(Sheet sheet,CellRangeAddress cellRangeAddress){
		return new StandardCell(sheet, cellRangeAddress);
	}
	
	public static final StandardCell valueOf(Cell cell){
		return new StandardCell(cell);
	}
	
	private final Sheet sheet;
	private final CellRangeAddress cellRangeAddress;
	private final Cell valueCell;
	
	private StandardCell(Sheet sheet,CellRangeAddress cellRangeAddress) {
		super();
		this.sheet=sheet;
		this.cellRangeAddress = cellRangeAddress;
		this.valueCell=ExcelUtil.getFirstCell(sheet, cellRangeAddress);
	}
	
	private StandardCell(Cell cell) {
		super();
		this.sheet=cell.getSheet();
		if(ExcelUtil.isMergedRegion(cell)){
			this.cellRangeAddress=ExcelUtil.getMergedRegion(cell);
			this.valueCell=ExcelUtil.getFirstCell(sheet, cellRangeAddress);
		}else{
			this.cellRangeAddress = new CellRangeAddress(cell.getRowIndex(), cell.getRowIndex(), cell.getColumnIndex(), cell.getColumnIndex());
			this.valueCell=cell;
		}
	}
	
	public Object getValue(){
		return ExcelUtil.getValue(valueCell);
	}
	
	public boolean isMergedRegion(){
		return cellRangeAddress.getNumberOfCells()>ONE;
	}
	
	public boolean containsColumn(int colInd) {
		return cellRangeAddress.containsColumn(colInd);
	}

	public boolean containsRow(int rowInd) {
		return cellRangeAddress.containsRow(rowInd);
	}

	public String formatAsString() {
		return cellRangeAddress.formatAsString();
	}

	public String formatAsString(String sheetName, boolean useAbsoluteAddress) {
		return cellRangeAddress.formatAsString(sheetName, useAbsoluteAddress);
	}

	public final int getFirstColumn() {
		return cellRangeAddress.getFirstColumn();
	}

	public final int getFirstRow() {
		return cellRangeAddress.getFirstRow();
	}

	public final int getLastColumn() {
		return cellRangeAddress.getLastColumn();
	}

	public final int getLastRow() {
		return cellRangeAddress.getLastRow();
	}

	public int getNumberOfCells() {
		return cellRangeAddress.getNumberOfCells();
	}

	public boolean intersects(CellRangeAddressBase other) {
		return cellRangeAddress.intersects(other);
	}

	public final boolean isFullColumnRange() {
		return cellRangeAddress.isFullColumnRange();
	}

	public final boolean isFullRowRange() {
		return cellRangeAddress.isFullRowRange();
	}

	public boolean isInRange(Cell cell) {
		return cellRangeAddress.isInRange(cell);
	}

	public boolean isInRange(CellReference ref) {
		return cellRangeAddress.isInRange(ref);
	}

	public boolean isInRange(int rowInd, int colInd) {
		return cellRangeAddress.isInRange(rowInd, colInd);
	}

	public void serialize(LittleEndianOutput out) {
		cellRangeAddress.serialize(out);
	}

	public final void setFirstColumn(int firstCol) {
		cellRangeAddress.setFirstColumn(firstCol);
	}

	public final void setFirstRow(int firstRow) {
		cellRangeAddress.setFirstRow(firstRow);
	}

	public final void setLastColumn(int lastCol) {
		cellRangeAddress.setLastColumn(lastCol);
	}

	public final void setLastRow(int lastRow) {
		cellRangeAddress.setLastRow(lastRow);
	}

	public void validate(SpreadsheetVersion ssVersion) {
		cellRangeAddress.validate(ssVersion);
	}

	public Sheet getSheet() {
		return sheet;
	}

	public CellRangeAddress getCellRangeAddress() {
		return cellRangeAddress;
	}

	public Cell getValueCell() {
		return valueCell;
	}
	
}
