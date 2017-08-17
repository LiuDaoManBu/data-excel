package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;

import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.util.ClassUtils;
import com.caotc.excel4j.util.ExcelUtil;

public class Data{
	private SheetParseResult sheetParseResult;
	private final Map<Menu,Cell> menuToValueCells;
	private final JSONObject jsonData=new JSONObject();
	
	public Data(Map<Menu, Cell> menuToCells) {
		super();
		this.menuToValueCells = menuToCells;
		for(Entry<Menu,Cell> entry:menuToCells.entrySet()){
			Menu menu=entry.getKey();
			Object dataValue=ExcelUtil.getValue(entry.getValue());
			if(dataValue!=null){
				jsonData.put(menu.getName(), dataValue);
				if(menu.getMenuConfig()!=null){
					if(StringUtils.isNotBlank(menu.getMenuConfig().getFieldName())){
						jsonData.put(menu.getMenuConfig().getFieldName(), dataValue);
					}
//					if(StringUtils.isNotBlank(menu.getMatchName())){
//						jsonData.put(menu.getMatchName(), dataValue);
//					}
				}
			}
			
		}
	}
	
	public Cell getCell(String fieldName){
		return menuToValueCells.get(sheetParseResult.getMenu(fieldName));
	}
	
	public Map<Menu, Cell> getMenuToCells() {
		return menuToValueCells;
	}

	public JSONObject getJsonData() {
		return jsonData;
	}
	
	public <T> T toJavaObject(Class<T> type) {
		Map<String,Field> nameToFields=ClassUtils.getAllFieldStream(type).collect(Collectors.toMap(Field::getName, Function.identity()
				,(field1, field2) -> field1.getDeclaringClass().isAssignableFrom(field2.getDeclaringClass())?field2:field1));
		menuToValueCells.forEach((menu,cell)->{
			String key=menu.getCheckMenuConfig().getFieldName();
			Object value=ExcelUtil.getValue(cell);
			if(nameToFields.containsKey(key)) {
				Class<?> fieldType=nameToFields.get(key).getType();
				if(!fieldType.equals(value.getClass())) {
					jsonData.put(key, menu.getCheckMenuConfig().getDataMatcher().getDataType().cast(value, fieldType));
				}
			}
		});
		return jsonData.toJavaObject(type);
	}
}
