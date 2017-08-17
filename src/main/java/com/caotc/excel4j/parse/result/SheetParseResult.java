package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.parse.error.SheetError;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class SheetParseResult {
	private Sheet sheet;
	private SheetConfig sheetConfig;
	private List<SheetError> sheetErrors;
	private Collection<Menu> menus=Lists.newArrayList();
	private Map<String,Menu> fieldNameToMenus=Maps.newHashMap();
	private Collection<Menu> fixedMenus=Lists.newArrayList();
	private Collection<Menu> noFixedMenus=Lists.newArrayList();
	private Collection<Menu> dynamicMenus=Lists.newArrayList();
	private Data fiexdData;
	private Collection<Data> noFiexdDatas=Lists.newArrayList();
	private Collection<Data> datas=Lists.newArrayList();
	
	public SheetParseResult(Sheet sheet,SheetConfig sheetConfig){
		this.sheet=sheet;
		this.sheetConfig=sheetConfig;
		findMenus();
		checkMenus();
	}
	
	public boolean hasError(){
		return !errors.isEmpty();
	}
	
	public Error getFirstError(){
		if(CollectionUtils.isEmpty(errors)){
			return null;
		}
		return errors.iterator().next();
	}
	
	public Menu getMenu(String fieldName){
		return fieldNameToMenus.get(fieldName);
	}
	
	public boolean hasMenu(String fieldName){
		return fieldNameToMenus.containsKey(fieldName);
	}
	
	public JSONArray getJsonDatas(){
		JSONArray array=new JSONArray();
		for(Data data:datas){
			array.add(data.getJsonData());
		}
		return array;
	}
	
	public <T> Collection<T> getClassDatas(Collection<T> collection,Class<T> clazz){
		for(Data data:datas){
			collection.add(JSONObject.toJavaObject(data.getJsonData(), clazz));
		}
		return collection;
	}
	
	public boolean addError(Error error){
		if(!errors.contains(error)){
			return errors.add(error);
		}
		return Boolean.FALSE;
	}
	
	public Error addError(String errorMessage){
		Error error= new Error(errorMessage);
		addError(error);
		return error;
	}
	
	public Error addError(Cell cell, String errorMessage){
		Error error= new Error(cell,errorMessage);
		addError(error);
		return error;
	}
	
	public boolean addMenu(Menu menu){
		boolean result=Boolean.FALSE;
		if(!menus.contains(menu)){
			result= menus.add(menu);
			if(menu.getMenuConfig()!=null && menu.getMenuConfig().getFieldName()!=null){
				fieldNameToMenus.put(menu.getMenuConfig().getFieldName(), menu);
			}
			MenuConfig menuConfig=menu.checkMenuConfig;
			if(menuConfig.isDataFlag() || menu.menuConfig==null){
				if(menuConfig.isSingleDataFlag()){
					fixedMenus.add(menu);
				}else{
					noFixedMenus.add(menu);
				}
				if(menuConfig.isDynamicFlag()){
					dynamicMenus.add(menu);
				}
			}
		}
		return result;
	}
	
	public Menu addMenu(Cell cell){
		Menu menu=new Menu(cell);
		addMenu(menu);
		return menu;
	}
	
	public Menu addMenu(Cell cell,MenuConfig menuConfig){
		Menu menu=new Menu(cell,menuConfig);
		addMenu(menu);
		return menu;
	}
	
	public void findMenus(){
		finParentMenus();
		findChildrenMenus();
	}
	
	public void finParentMenus(){
		for(int rowIndex=sheet.getFirstRowNum();rowIndex<=sheet.getLastRowNum();rowIndex++){
			Row row=sheet.getRow(rowIndex);
			for(int columnIndex=row.getFirstCellNum();columnIndex<row.getLastCellNum();columnIndex++){
				Cell cell=row.getCell(columnIndex);
				CellRangeAddress mergedRegion=ExcelUtil.getMergedRegion(cell);
				if(mergedRegion==null || (cell.getRowIndex()==mergedRegion.getFirstRow() && cell.getColumnIndex()
						==mergedRegion.getFirstColumn())){
					String value=ExcelUtil.getStringValue(cell);
//					for(MenuConfig menuConfig:sheetConfig.menuConfigs){
//						if(menuConfig.getParentMenuConfig()==null && menuConfig.getMenuNameMatcher().matches(value)){
//							addMenu(cell, menuConfig);
//							break;
//						}
//					}
				}
			}
		}
	}
	
	public void findChildrenMenus(){
		List<Menu> childrenMenus=Lists.newArrayList();
		for(Menu menu:menus){
			childrenMenus.addAll(menu.findChildrenMenus());
		}
		for(Menu childrenMenu:childrenMenus){
			addMenu(childrenMenu);
		}
	}
	
	public void checkMenus(){
		Map<MenuConfig,Menu> menuConfigToMenus=Maps.newHashMap();
		for(Menu menu:menus){
			if(menu.menuConfig!=null){
				menuConfigToMenus.put(menu.menuConfig, menu);
			}
		}
//		for(MenuConfig menuConfig:menuConfigs){
//			if(!menuConfigToMenus.containsKey(menuConfig) && menuConfig.getMustFlag()){
//				addError("请检查模板是否有误,工作簿"+sheet.getSheetName()+"未找到菜单:"+menuConfig.getMenuNameMatcher().getMatchString());
//			}
//		}
	}
	
	public void parseFixedMenuDatas(){
		Map<Menu, Cell> menuToCells=Maps.newHashMap();
		for(Menu fixedMenu:fixedMenus){
			Cell dataCell=fixedMenu.nextDataCell(fixedMenu.getCell());
			if(fixedMenu.hasCheckMenuConfig()){
				fixedMenu.checkDataCell(dataCell);
			}
			if(StringUtils.isNotBlank(ExcelUtil.getStringValue(dataCell))){
				menuToCells.put(fixedMenu, dataCell);
			}
		}
		fiexdData=new Data(menuToCells);
	}
	
	public void parseNoFixedMenuDatas(){
		if(!CollectionUtils.isEmpty(noFixedMenus)){
			Menu firstNoFixedMenu=noFixedMenus.iterator().next();
			Cell currentFirstNoFixedMenuDataCell=firstNoFixedMenu.nextDataCell(firstNoFixedMenu.getCell());
//			for(int i=1;ExcelUtil.isDataCell(currentFirstNoFixedMenuDataCell,firstNoFixedMenu,menus);i++){
//				Map<Menu,Cell> menuToCells=Maps.newHashMap();
//				for(Menu noFixedMenu:noFixedMenus){
//					Cell dataCell=noFixedMenu.getDataCell(i);
//					menuToCells.put(noFixedMenu,dataCell);
//				}
//				if(!CollectionUtils.isEmpty(menuToCells)){
//					noFiexdDatas.add(new Data(menuToCells));
//				}
//				currentFirstNoFixedMenuDataCell=firstNoFixedMenu.nextDataCell(currentFirstNoFixedMenuDataCell);
//			}
			for (Iterator<Data> iter = noFiexdDatas.iterator(); iter.hasNext();) {  
				Data data=iter.next();
				if(data.jsonData.isEmpty()){
					iter.remove();
				}else{
					for(Entry<Menu,Cell> entry:data.menuToCells.entrySet()){
						Menu menu=entry.getKey();
						Cell dataCell=entry.getValue();
						if(menu.hasCheckMenuConfig()){
							menu.checkDataCell(dataCell);
						}
					}
				}
			}  
		}
	}
	
	public void parseDatas(){
		parseFixedMenuDatas();
		parseNoFixedMenuDatas();
		for(Data noFiexData:noFiexdDatas){
			Map<Menu,Cell> menuToCells=Maps.newHashMap(noFiexData.menuToCells);
			menuToCells.putAll(fiexdData.menuToCells);
			datas.add(new Data(menuToCells));
		}
	}
	
	public Collection<Menu> getMenus() {
		return menus;
	}

	public void setMenus(List<Menu> menus) {
		this.menus = menus;
	}
	
	public Collection<Menu> getFixedMenus() {
		return fixedMenus;
	}

	public void setFixedMenus(List<Menu> fixedMenus) {
		this.fixedMenus = fixedMenus;
	}

	public Collection<Menu> getNoFixedMenus() {
		return noFixedMenus;
	}

	public void setNoFixedMenus(List<Menu> noFixedMenus) {
		this.noFixedMenus = noFixedMenus;
	}

	public Collection<Error> getErrors() {
		return errors;
	}

	public void setErrors(List<Error> errors) {
		this.errors = errors;
	}

	public Data getFiexdData() {
		return fiexdData;
	}

	public void setFiexdData(Data fiexdData) {
		this.fiexdData = fiexdData;
	}

	public Collection<Data> getNoFiexdDatas() {
		return noFiexdDatas;
	}

	public void setNoFiexdDatas(List<Data> noFiexdDatas) {
		this.noFiexdDatas = noFiexdDatas;
	}

	public Collection<Data> getDatas() {
		return datas;
	}

	public void setDatas(List<Data> datas) {
		this.datas = datas;
	}

	public Collection<Menu> getDynamicMenus() {
		return dynamicMenus;
	}

	public void setDynamicMenus(List<Menu> dynamicMenus) {
		this.dynamicMenus = dynamicMenus;
	}

	public Sheet getSheet() {
		return sheet;
	}

	public void setSheet(Sheet sheet) {
		this.sheet = sheet;
	}

//	public Collection<SheetConfig> getMenuConfigs() {
//		return menuConfigs;
//	}
//
//	public void setMenuConfigs(Collection<SheetConfig> menuConfigs) {
//		this.menuConfigs = menuConfigs;
//	}
	
}
