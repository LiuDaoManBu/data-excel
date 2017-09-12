package com.caotc.excel4j.parse.result;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.util.CellRangeAddress;

import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.Lists;

public class Menu{
	private StandardCell cell;
	private MenuConfig menuConfig;
	private MenuConfig checkMenuConfig;
	private Menu parentMenu;
	private List<Menu> childrenMenus=Lists.newArrayList();
	
	public Menu(StandardCell cell) {
		super();
		this.cell = cell;
	}

	public Menu(StandardCell cell, MenuConfig menuConfig) {
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
	
	public void checkDataCell(StandardCell dataCell){
		if(hasCheckMenuConfig()){
			Object value=dataCell.getValue();
//			if(!checkMenuConfig.getDataMatcher().matches(value)){
//				StringBuffer errorMessage=new StringBuffer();
//				errorMessage.append("工作簿").append(dataCell.getSheet().getSheetName()).append("第").append(dataCell
//						.getRowIndex()+1).append("行").append(ExcelUtil.indexToColumn(dataCell.getColumnIndex()+1))
//						.append("列的").append(getName()).append("数据格式不正确");
//				addError(errorMessage.toString());
//			}
			
//			if(StringUtils.isEmpty(cellString)){
//				if(checkMenuConfig.isCheckEmpty()){
//					errorMessage.append("不能为空");
//					addError(errorMessage.toString());
//				}
//			}else{
//				Integer maxLength=checkMenuConfig.getMaxLength();
//				if(maxLength!=null && maxLength>=0 && cellString.length()>maxLength){
//					errorMessage.append("长度超过").append(checkMenuConfig.getMaxLength());
//					addError(errorMessage.toString());
//				}
//				
//				String regex=checkMenuConfig.getRegex();
//				if(regex!=null){
//					regex+=checkMenuConfig.isCheckEmpty()?"+":"*";
//					if(!cellString.matches(regex)){
//						String regexTip=ExcelUtil.REGEX_AND_TIP_MAP.get(checkMenuConfig.getRegex());
//						if(regexTip!=null){
//							errorMessage.append("数据格式不正确，应为").append(regexTip);
//						}else{
//							errorMessage.append("数据格式不正确");
//						}
//						addError(errorMessage.toString());
//					}
//				}
//			}
		}
	}
	
	public StandardCell getDataCell(int serialNumber){
		StandardCell dataCell=cell;
		for(int i=1;i<=serialNumber;i++){
			dataCell=nextDataCell(dataCell);
		}
		return dataCell;
	}
	
	public StandardCell nextDataCell(StandardCell cell){
		Direction direction=checkMenuConfig.getDirection();
		if(cell==null || this.cell.equals(cell)){
			return direction.getCell(this.cell, checkMenuConfig.getFirstDistance());
		}
		return direction.nextCell(cell);
	}
	
	public boolean hasCheckMenuConfig(){
		return checkMenuConfig!=null;
	}
	
	public MenuConfig getCheckMenuConfig() {
		return checkMenuConfig;
	}

	public void setCheckMenuConfig(MenuConfig checkMenuConfig) {
		this.checkMenuConfig = checkMenuConfig;
	}

	public List<Menu> findChildrenMenus(){
		List<Menu> childrenMenus=Lists.newArrayList();
		if(menuConfig!=null){
			if(menuConfig.getDynamic() || !CollectionUtils.isEmpty(menuConfig.getChildrenMenuConfigs())){
				StandardCell menuCell=this.cell;
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
						StandardCell cell=ExcelUtil.getCellByIndex(menuCell.getSheet(), rowIndex, columnIndex);
						if(!childrenMenus.contains(cell)){
							CellRangeAddress mergedRegion=ExcelUtil.getMergedRegion(cell);
							if(mergedRegion==null || (cell.getRowIndex()==mergedRegion.getFirstRow() && cell.getColumnIndex()
									==mergedRegion.getFirstColumn())){
								Menu childrenMenu=null;
//								for(MenuConfig childrenMenuConfig:menuConfig.getChildrenMenuConfigs()){
//									if(childrenMenuConfig.getMenuNameMatcher().matches(ExcelUtil.getStringValue(cell))){
//										childrenMenu=new Menu(cell,childrenMenuConfig);
//									}
//								}
								if(childrenMenu==null && menuConfig.getDynamic()){
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
		return cell.getValueCell().getStringCellValue();
	}
	
//	public String getMatchName(){
//		return menuConfig.getMenuNameMatcher().getMatchString();
//	}
	
	public StandardCell getCell() {
		return cell;
	}
	public void setCell(StandardCell cell) {
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
