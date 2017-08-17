package com.caotc.util.excel.parse.error;

import org.apache.poi.ss.usermodel.Sheet;

public class SheetError {
	private Sheet sheet;
	private String errorMessage;
	
	public SheetError(String errorMessage) {
		this.errorMessage = errorMessage;
	}
	public SheetError(Sheet sheet, String errorMessage) {
		this.sheet = sheet;
		this.errorMessage = errorMessage;
	}
	public Sheet getSheet() {
		return sheet;
	}
	public void setSheet(Sheet sheet) {
		this.sheet = sheet;
	}
	public String getErrorMessage() {
		return errorMessage;
	}
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}
}
