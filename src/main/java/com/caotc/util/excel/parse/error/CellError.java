package com.caotc.util.excel.parse.error;

import org.apache.poi.ss.usermodel.Cell;

public class CellError {
	private Cell cell;
	private String errorMessage;
	
	public CellError(String errorMessage) {
		this.errorMessage = errorMessage;
	}
	public CellError(Cell cell, String errorMessage) {
		this.cell = cell;
		this.errorMessage = errorMessage;
	}
	public Cell getCell() {
		return cell;
	}
	public void setCell(Cell cell) {
		this.cell = cell;
	}
	public String getErrorMessage() {
		return errorMessage;
	}
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}
}
