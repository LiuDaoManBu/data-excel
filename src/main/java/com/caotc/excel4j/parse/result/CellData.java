package com.caotc.excel4j.parse.result;

import org.apache.poi.ss.usermodel.Cell;

import com.caotc.excel4j.util.ExcelUtil;

public class CellData {
	private Menu menu;
	private Cell valueCell;
	
	public Object getValue() {
		return ExcelUtil.getValue(valueCell);
	}
	
	public <T> T getValue(Class<T> type) {
		if(type==null) {
			throw new IllegalArgumentException("class type can't be null");
		}
		Object value=getValue();
		if(!type.equals(value.getClass())) {
			
		}
	}
}
