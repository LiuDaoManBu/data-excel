package com.caotc.util.excel.parse.result;

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
import com.caotc.util.excel.ExcelUtil;
import com.caotc.util.excel.config.MenuConfig;
import com.caotc.util.excel.config.SheetConfig;
import com.caotc.util.excel.config.TableConfig.Direction;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class SheetParseResult {
	private Sheet sheet;
	private SheetConfig sheetConfig;
	private Collection<Menu> menus=Lists.newArrayList();
	private Map<String,Menu> fieldNameToMenus=Maps.newHashMap();
	private Collection<Menu> fixedMenus=Lists.newArrayList();
	private Collection<Menu> noFixedMenus=Lists.newArrayList();
	private Collection<Menu> dynamicMenus=Lists.newArrayList();
	private Collection<Error> errors=Lists.newArrayList();
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


	public class Error{
		private Cell cell;
		private String errorMessage;
		
		public Error(String errorMessage) {
			super();
			this.errorMessage = errorMessage;
		}
		public Error(Cell cell, String errorMessage) {
			super();
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
	
	public class Data{
		private final Map<Menu,Cell> menuToCells;
		private final JSONObject jsonData=new JSONObject();
		
		public Data(Map<Menu, Cell> menuToCells) {
			super();
			this.menuToCells = menuToCells;
			for(Entry<Menu,Cell> entry:menuToCells.entrySet()){
				Menu menu=entry.getKey();
				Object dataValue=ExcelUtil.getValue(entry.getValue());
				if(dataValue!=null){
					jsonData.put(menu.getName(), dataValue);
					if(menu.getMenuConfig()!=null){
						if(StringUtils.isNotBlank(menu.getMenuConfig().getFieldName())){
							jsonData.put(menu.getMenuConfig().getFieldName(), dataValue);
						}
//						if(StringUtils.isNotBlank(menu.getMatchName())){
//							jsonData.put(menu.getMatchName(), dataValue);
//						}
					}
				}
				
			}
		}
		
		public Cell getCell(String fieldName){
			return menuToCells.get(getMenu(fieldName));
		}
		
		public Map<Menu, Cell> getMenuToCells() {
			return menuToCells;
		}

		public JSONObject getJsonData() {
			return jsonData;
		}
	}
	
	public class Menu{
		private Cell cell;
		private MenuConfig menuConfig;
		private MenuConfig checkMenuConfig;
		private Menu parentMenu;
		private List<Menu> childrenMenus=Lists.newArrayList();
		
		public Menu(Cell cell) {
			super();
			this.cell = cell;
		}

		public Menu(Cell cell, MenuConfig menuConfig) {
			super();
			this.cell = cell;
			this.menuConfig = menuConfig;
			checkMenuConfig=menuConfig;
			Menu lastMenu=parentMenu;
			while(checkMenuConfig==null && lastMenu!=null){
				checkMenuConfig=lastMenu.menuConfig;
				lastMenu=parentMenu.getParentMenu();
			}
		}
		
		public void checkDataCell(Cell dataCell){
			if(hasCheckMenuConfig()){
				Object value=ExcelUtil.getValue(dataCell);
//				if(!checkMenuConfig.getDataMatcher().matches(value)){
//					StringBuffer errorMessage=new StringBuffer();
//					errorMessage.append("工作簿").append(dataCell.getSheet().getSheetName()).append("第").append(dataCell
//							.getRowIndex()+1).append("行").append(ExcelUtil.indexToColumn(dataCell.getColumnIndex()+1))
//							.append("列的").append(getName()).append("数据格式不正确");
//					addError(errorMessage.toString());
//				}
				
//				if(StringUtils.isEmpty(cellString)){
//					if(checkMenuConfig.isCheckEmpty()){
//						errorMessage.append("不能为空");
//						addError(errorMessage.toString());
//					}
//				}else{
//					Integer maxLength=checkMenuConfig.getMaxLength();
//					if(maxLength!=null && maxLength>=0 && cellString.length()>maxLength){
//						errorMessage.append("长度超过").append(checkMenuConfig.getMaxLength());
//						addError(errorMessage.toString());
//					}
//					
//					String regex=checkMenuConfig.getRegex();
//					if(regex!=null){
//						regex+=checkMenuConfig.isCheckEmpty()?"+":"*";
//						if(!cellString.matches(regex)){
//							String regexTip=ExcelUtil.REGEX_AND_TIP_MAP.get(checkMenuConfig.getRegex());
//							if(regexTip!=null){
//								errorMessage.append("数据格式不正确，应为").append(regexTip);
//							}else{
//								errorMessage.append("数据格式不正确");
//							}
//							addError(errorMessage.toString());
//						}
//					}
//				}
			}
		}
		
		public Cell getDataCell(int serialNumber){
			Cell dataCell=cell;
			for(int i=1;i<=serialNumber;i++){
				dataCell=nextDataCell(dataCell);
			}
			return dataCell;
		}
		
		public Cell nextDataCell(Cell cell){
			Direction direction=checkMenuConfig.getDirection();
			if(cell==null || this.cell.equals(cell)){
				return direction.getCell(this.cell, checkMenuConfig.getFirstDataDistance());
			}
			return direction.nextCell(cell);
		}
		
		public boolean hasCheckMenuConfig(){
			return checkMenuConfig!=null;
		}
		
//		public SheetConfig getCheckMenuConfig() {
//			return checkMenuConfig;
//		}
//
//		public void setCheckMenuConfig(SheetConfig checkMenuConfig) {
//			this.checkMenuConfig = checkMenuConfig;
//		}

		public List<Menu> findChildrenMenus(){
			List<Menu> childrenMenus=Lists.newArrayList();
			if(menuConfig!=null){
				if(menuConfig.isDynamicFlag() || !CollectionUtils.isEmpty(menuConfig.getChildrenMenuConfigs())){
					Cell menuCell=this.cell;
					CellRangeAddress menuMergedRegion=ExcelUtil.getMergedRegion(menuCell);
					int startRowIndex,endRowIndex,startColumnIndex,endColumnIndex;
					if(menuMergedRegion==null){
						startRowIndex=menuCell.getRowIndex();
						endRowIndex=menuCell.getRowIndex();
						startColumnIndex=menuCell.getColumnIndex();
						endColumnIndex=menuCell.getColumnIndex();
					}else{
						startRowIndex=menuMergedRegion.getFirstRow();
						endRowIndex=menuMergedRegion.getLastRow();
						startColumnIndex=menuMergedRegion.getFirstColumn();
						endColumnIndex=menuMergedRegion.getLastColumn();
					}
					switch(menuConfig.getDirection()){
					case TOP:startRowIndex--;endRowIndex--;break;
					case BOTTOM:startRowIndex++;endRowIndex++;break;
					case LEFT:startColumnIndex--;endColumnIndex--;break;
					default:startColumnIndex++;endColumnIndex++;break;
					}
					for(int rowIndex=startRowIndex;rowIndex<=endRowIndex;rowIndex++){
						for(int columnIndex=startColumnIndex;columnIndex<=endColumnIndex;columnIndex++){
							Cell cell=ExcelUtil.getCellByIndex(menuCell.getSheet(), rowIndex, columnIndex);
							if(!childrenMenus.contains(cell)){
								CellRangeAddress mergedRegion=ExcelUtil.getMergedRegion(cell);
								if(mergedRegion==null || (cell.getRowIndex()==mergedRegion.getFirstRow() && cell.getColumnIndex()
										==mergedRegion.getFirstColumn())){
									Menu childrenMenu=null;
//									for(MenuConfig childrenMenuConfig:menuConfig.getChildrenMenuConfigs()){
//										if(childrenMenuConfig.getMenuNameMatcher().matches(ExcelUtil.getStringValue(cell))){
//											childrenMenu=new Menu(cell,childrenMenuConfig);
//										}
//									}
									if(childrenMenu==null && menuConfig.isDynamicFlag()){
										childrenMenu=new Menu(cell);
									}
									if(childrenMenu!=null){
										childrenMenu.setParentMenu(this);
										childrenMenus.add(childrenMenu);
										childrenMenus.addAll(childrenMenu.findChildrenMenus());
									}
								}
							}
						}
					}
				}
			}
			return childrenMenus;
		}
		
		public String getName(){
			return cell.getStringCellValue();
		}
		
//		public String getMatchName(){
//			return menuConfig.getMenuNameMatcher().getMatchString();
//		}
		
		public Cell getCell() {
			return cell;
		}
		public void setCell(Cell cell) {
			this.cell = cell;
		}
		public MenuConfig getMenuConfig() {
			return menuConfig;
		}
		public void setMenuConfig(MenuConfig menuConfig) {
			this.menuConfig = menuConfig;
		}

		public Menu getParentMenu() {
			return parentMenu;
		}

		public void setParentMenu(Menu parentMenu) {
			this.parentMenu = parentMenu;
			if(!parentMenu.getChildrenMenus().contains(this)){
				parentMenu.getChildrenMenus().add(this);
			}
		}

		public List<Menu> getChildrenMenus() {
			return childrenMenus;
		}

		public void setChildrenMenus(List<Menu> childrenMenus) {
			this.childrenMenus = childrenMenus;
			if(!CollectionUtils.isEmpty(childrenMenus)){
				for(Menu childrenMenu:childrenMenus){
					childrenMenu.setParentMenu(this);
				}
			}
		}
	}
	
}
