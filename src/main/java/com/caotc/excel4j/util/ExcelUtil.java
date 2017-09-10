package com.caotc.excel4j.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;

import com.caotc.excel4j.config.SheetConfig;
import com.caotc.excel4j.config.WorkbookConfig;
import com.caotc.excel4j.parse.result.SheetParseResult;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.collect.Sets;


public class ExcelUtil {
	private static final SimpleDateFormat DEFAULT_DATE_FORMAT=new SimpleDateFormat("yyyy-MM-dd");
	//正数的正则表达式
	public static final String POSITIVE_NUMBER="((([1-9]\\d*)|(0))(\\.\\d+)?)";
	//数字的正则表达式
	public static final String NUMBER="(-?(([1-9]\\d*)|(0))(\\.\\d+)?)";
	//小于一的正小数的正则表达式
	public static final String LESS_ONE_DECIMAL_REGEX="(0.\\d+)";
	//小于等于一的正小数的正则表达式
	public static final String LESS_OR_EQUAL_ONE_DECIMAL="((0.\\d+)|1)";
	//汉字英文数字的正则表达式
	public static final String CHINESE_OR_ENGLISH_OR_NUMBER="([\u0391-\uFFE5]|[A-Za-z]|\\d)";
	//汉字的正则表达式
	public static final String CHINESE="([\u0391-\uFFE5])";
	//汉字英语的正则表达式
	public static final String CHINESE_OR_ENGLISH="([\u0391-\uFFE5]|[A-Za-z])";
	//自然数的正则表达式
	public static final String POSITIVE_INTEGER="(\\d)";
	//电话号码的正则表达式
	public static final String PHONE_NUMBER="(((\\d{3,4})|\\d{3,4}-)?\\d{7,8})";
	//多级菜单名的分隔符
	public static final String MENU_SEPARATOR="_";
	//英文数字的正则表达式
	public static final String ENGLISH_OR_NUMBER="([A-Za-z]|\\d)";
	//时间的正则表达式
	public static final String TIME ="(^((?:19|20)\\d\\d)(-|/)(0[1-9]|1[012])(-|/)(0[1-9]|[12][0-9]|3[01])$)";
    
	public static final String RPT1_REGISTER_TIME ="(^((?:19|20)\\d\\d)(-|/)([1-9]|0[1-9]|1[012])(-|/)([1-9]|0[1-9]|[12][0-9]|3[01])$)";
	
	//正则表达式与对应的提示语
	public static Map<String,String> REGEX_AND_TIP_MAP=new HashMap<String,String>();
	static{
		REGEX_AND_TIP_MAP.put(POSITIVE_NUMBER, "正数");
		REGEX_AND_TIP_MAP.put(LESS_ONE_DECIMAL_REGEX, "小于一的正小数或者百分比");
		REGEX_AND_TIP_MAP.put(LESS_OR_EQUAL_ONE_DECIMAL, "小于等于一的正小数或者百分比");
		REGEX_AND_TIP_MAP.put(CHINESE_OR_ENGLISH_OR_NUMBER, "汉字英文数字");
		REGEX_AND_TIP_MAP.put(CHINESE, "汉字");
		REGEX_AND_TIP_MAP.put(CHINESE_OR_ENGLISH, "汉字英文");
		REGEX_AND_TIP_MAP.put(POSITIVE_INTEGER, "自然数");
		REGEX_AND_TIP_MAP.put(NUMBER, "数字");
		REGEX_AND_TIP_MAP.put(PHONE_NUMBER, "电话号码的格式应为“XXXX-XXXXXXX”，“XXXX-XXXXXXXX”，“XXX-XXXXXXX”，“XXX-XXXXXXXX”，“XXXXXXX”，“XXXXXXXX”");
		REGEX_AND_TIP_MAP.put(ENGLISH_OR_NUMBER, "英文数字");
		REGEX_AND_TIP_MAP.put(TIME, "日期格式为YYYY-MM-dd或YYYY/MM/dd");
		REGEX_AND_TIP_MAP.put(RPT1_REGISTER_TIME, "日期格式为YYYY/MM/dd或YYYY/MM/d或YYYY/M/dd或YYYY/M/d");
	}
	
	private static final SimpleDateFormat SDF1=new SimpleDateFormat("yyyy/MM/dd");
	private static final SimpleDateFormat SDF2=new SimpleDateFormat("yyyy-MM-dd");

	/**
	 * 根据行坐标和列坐标获取单元格内的内容，并以字符串的形式返回
	 * @author caotc
	 * @date 2016.4.24
	 * @param sheet 工作簿
	 * @param rowIndex 行坐标
	 * @param columnIndex 列坐标
	 * @return 单元格内容的字符串，无该单元格返回null
	 */
	public static String getStringCellValue(Sheet sheet, int rowIndex,int columnIndex) {
		Cell cell=sheet.getRow(rowIndex).getCell(columnIndex);
		return getStringValue(cell);
	}
	
	public static void parseWorkbook(Workbook workbook,WorkbookConfig workbookConfig){
		
	}
	
	public static void parseSheet(Sheet sheet,SheetConfig sheetConfig){
		
	}
	
	/**
	 * 检查传入的excel对象中是否有该名字的工作簿以及是否有有效内容
	 * @author caotc
	 * @date 2016.4.24
	 * @param workbook excel对象
	 * @param sheetName 工作簿名字
	 * @return 错误信息字符串，没有则为null
	 */
	public static String checkSheet(Workbook workbook,String sheetName){
		Sheet sheet=workbook.getSheet(sheetName);
		if(sheet==null){
			return "不存在名为"+sheetName+"的工作簿";
		}
		int firstRowNum = sheet.getFirstRowNum();
		int lastRowNum = sheet.getLastRowNum();
		if((lastRowNum - 1) < firstRowNum){
			return sheetName+"工作簿不存在有效数据";
		}
		return null;
	}
	//TODO 转移到Menu类中?
//	public static boolean isDataCell(Cell cell,Menu menu,Collection<Menu> menus){
//		if(cell==null){
//			return false;
//		}
//		CellRangeAddress cellRangeAddress=getMergedRegion(cell);
//		if(cellRangeAddress!=null){
//			return false;
//		}
//		
//		String cellString=getStringValue(cell);
//		if(StringUtils.isEmpty(cellString)){
//			CellStyle cellStyle=cell.getCellStyle();
//			short boderType=CellStyle.BORDER_NONE;
//			switch(menu.getCheckMenuConfig().getDirection()){
//			case TOP:boderType=cellStyle.getBorderTop();break;
//			case BOTTOM:boderType=cellStyle.getBorderBottom();break;
//			case LEFT:boderType=cellStyle.getBorderLeft();break;
//			default:boderType=cellStyle.getBorderRight();
//			}
//			if(boderType==CellStyle.BORDER_NONE){
//				return false;
//			}
//		}
//		
//		List<Cell> menuCells=Lists.newArrayList();
//		for(Menu m:menus){
//			menuCells.add(m.getCell());
//		}
//		
//		if(menuCells.contains(cell)){
//			return false;
//		}
//		return true;
//	}
	
	public static SheetParseResult parseMenu(Sheet sheet,SheetConfig sheetConfig){
		SheetParseResult result=new SheetParseResult(sheet,sheetConfig);
		return result;
	}
	
	public static SheetParseResult parse(Sheet sheet,SheetConfig sheetConfig){
		SheetParseResult result=parseMenu(sheet,sheetConfig);
		result.parseDatas();
		return result;
	}
	
	/**
	 * 根据数据单元格数目并非固定的菜单数据检查配置对象集合将工作簿的的数据处理为一个Map的集合，一个map为一条数据
	 * @author caotc
	 * @date 2016.4.24
	 * @param sheet 工作簿
	 * @param menuDataCheckConfigList 数据单元格数目并非固定的菜单数据检查配置对象集合
	 * @return 数据的集合
	 */
	//TODO 改为直接解析后获取数据
//	public static List<Map<String,String>> getNoFixedDatas(Sheet sheet,Collection<SheetConfig> menuDataCheckConfigList) {
//		List<SheetConfig> noFixedDataConfigs= new ArrayList<SheetConfig>();
//		for(SheetConfig menuDataCheckConfig:menuDataCheckConfigList){
//			if(menuDataCheckConfig.isDataFlag() && !menuDataCheckConfig.isSingleDataFlag()){
//				noFixedDataConfigs.add(menuDataCheckConfig);
//			}
//		}
//		List<Map<String,String>> list=new ArrayList<Map<String,String>>();
//		Map<SheetConfig,Cell> lastCellMap=new HashMap<SheetConfig,Cell>();
//		Map<String,String> map=new LinkedHashMap<String,String>();
//		for(SheetConfig menuDataCheckConfig:noFixedDataConfigs){
//			Cell menuCell=getCellByMenuName(sheet, menuDataCheckConfig.getMenuNameMatcher().getMatchString());
//			Cell dataCell=menuDataCheckConfig.getDirection().nextCell(menuCell);
//			checkAndAddDataCell(dataCell,menuDataCheckConfig,menuDataCheckConfigList,lastCellMap,map);
//		}
//		while(!map.isEmpty()){
//			list.add(map);
//			map=new LinkedHashMap<String,String>();
//			for(SheetConfig menuDataCheckConfig:noFixedDataConfigs){
//				Cell lastDataCell=lastCellMap.get(menuDataCheckConfig);
//				Cell dataCell=menuDataCheckConfig.getDirection().nextCell(lastDataCell);
//				checkAndAddDataCell(dataCell,menuDataCheckConfig,menuDataCheckConfigList,lastCellMap,map);
//			}
//		}
//		return list;
//	}
	
	/**
	 * 在工作簿中根据传入的数据单元格数目固定的菜单数据检查配置对象得到该菜单数据检查配置对象的数据单元格集合
	 * @author caotc
	 * @date 2016.4.24
	 * @param sheet 工作簿
	 * @param menuDataCheckConfig 数据单元格数目固定的菜单数据检查配置对象
	 * @return 该菜单数据检查配置对象的数据单元格集合
	 */
	//TODO 改为直接解析后获取数据
//	public static String getFixedDatas(Sheet sheet,SheetConfig menuDataCheckConfig) {
//		Cell menuCell=getCellByMenuName(sheet, menuDataCheckConfig.getMenuNameMatcher().getMatchString());
//		Cell dataCell=menuDataCheckConfig.getDirection().nextCell(menuCell);
//		return getStringValue(dataCell);
//	}
	
	//TODO 解决报错
//	public static final JSONArray getDatas(Sheet sheet,SheetConfig sheetConfig){
//		JSONArray datas=new JSONArray();
//		List<Map<String,String>> noFixedDatas=getNoFixedDatas(sheet, menuDataCheckConfigs);
//		Map<String,String> fixedDatas=Maps.newHashMap();
//		for(SheetConfig menuDataCheckConfig:menuDataCheckConfigs){
//			if(menuDataCheckConfig.isDataFlag() && menuDataCheckConfig.isSingleDataFlag()){
//				String value=getFixedDatas(sheet,menuDataCheckConfig);
//				fixedDatas.put(menuDataCheckConfig.getMenuNameMatcher().getMatchString(), value);
//			}
//		}
//		
//		for(Map<String,String> noFixedData:noFixedDatas){
//			JSONObject json=new JSONObject();
//			json.putAll(noFixedData);
//			json.putAll(fixedDatas);
//			datas.add(json);
//		}
//		return datas;
//	}
	
	//TODO 解决报错
//	 public static final <T> List<T> getDatas(Sheet sheet,SheetConfig sheetConfig, Class<T> clazz) {
//		 JSONArray datas=getDatas(sheet, menuDataCheckConfigs);
//		 List<T> javaDatas=Lists.newArrayList();
//		 for(int i=0;i<datas.size();i++){
//			 javaDatas.add(JSONObject.toJavaObject(datas.getJSONObject(i), clazz));
//		 }
//		 return javaDatas;
//	 }
	
	/**
	 * 在传入的工作簿中根据行坐标和列坐标获得单元格
	 * @author caotc
	 * @date 2016.4.24
	 * @param sheet 工作簿
	 * @param rowIndex 行坐标
	 * @param columnIndex 列坐标
	 * @return 单元格，不存在则为null
	 */
	public static Cell getCellByIndex(Sheet sheet,int rowIndex,int columnIndex) {
		Row row=sheet.getRow(rowIndex);
		if(row==null){
			row=sheet.createRow(rowIndex);
		}
		Cell cell=row.getCell(columnIndex,MissingCellPolicy.CREATE_NULL_AS_BLANK);
		return cell;
	}
	
	public static StandardCell getStandardCellByIndex(Sheet sheet,int rowIndex,int columnIndex) {
		return StandardCell.valueOf(getCellByIndex(sheet,rowIndex,columnIndex));
	}
	
	public static boolean isMergedRegion(Cell cell) {
		return getMergedRegion(cell)==null;
	}
	
	/**
	 * 检查传入的单元格是否为合并单元格，是则返回该合并单元格对象
	 * @author caotc
	 * @date 2016.4.24
	 * @param cell 单元格
	 * @return 该合并单元格对象，不是则为null
	 */
	public static CellRangeAddress getMergedRegion(Cell cell) {
		if(cell==null){
			return null;
		}
		return getMergedRegion(cell.getSheet(),cell.getRowIndex(),cell.getColumnIndex());
	}
	
	/**  
	* 判断指定的单元格是否是合并单元格，是则返回该合并单元格对象
	* @author caotc
	* @date 2016.4.24
	* @param sheet 工作簿
	* @param rowIndex 行下标  
	* @param columnIndex 列下标  
	* @return 该合并单元格对象，不是则为null
	*/  
	public static CellRangeAddress s(Sheet sheet,int rowIndex ,int columnIndex) {  
		if(sheet!=null){
			int sheetMergeCount = sheet.getNumMergedRegions(); 
			for (int i = 0; i < sheetMergeCount; i++) {  
				CellRangeAddress range = sheet.getMergedRegion(i);  
				if(range.isInRange(rowIndex, columnIndex)){
					return range;
				}
			}
		}
		return null;  
	}  
	
	/**  
	* 判断传入的工作簿中是否含有合并单元格
	* @author caotc
	* @date 2016.4.24
	* @param sheet 工作簿
	* @return 是否含有合并单元格
	*/  
	public static boolean hasMerged(Sheet sheet) {  
	        return sheet.getNumMergedRegions() > 0;
	} 
	
	public static void setDataFromEntity(Sheet sheet,List<?> datas,Map<String,SheetConfig> map,SimpleDateFormat sdf){
//		if(datas!=null && !datas.isEmpty()){
//			for(Entry<String,SheetConfig> entry:map.entrySet()){
//				Field field=null;
//				try {
//					field=datas.get(0).getClass().getDeclaredField(entry.getKey());
//					if(field!=null){
//						field.setAccessible(true);
//						SheetConfig config=entry.getValue();
//						if(config.isDataFlag()){
//							if(config.isSingleDataFlag()){
//								Cell menuCell=getCellByMenuName(sheet, config.getMenuNameMatcher().getMatchString());
//								if(menuCell!=null){
//									Cell dataCell=config.getDirection().nextCell(menuCell);
//									if(dataCell!=null){
//										Object value=field.get(datas.get(0));
//										setCellValue(dataCell,value,sdf);
//									}
//								}
//							}else{
//								Cell menuCell=getCellByMenuName(sheet, config.getMenuNameMatcher().getMatchString());
//								if(menuCell!=null){
//									CellStyle cellStyle=config.getDirection().nextCell(menuCell).getCellStyle();
//									Cell dataCell=null;
//									for(int i=0;i<datas.size();i++){
//										dataCell=config.getDirection().nextCell(menuCell);
//										Object value=field.get(datas.get(i));
//										setCellValue(dataCell,value,sdf);
//										dataCell.setCellStyle(cellStyle);
//									}
//								}
//							}
//						}
//						field.setAccessible(false);
//					}
//				} catch (Exception e) {
//					e.printStackTrace();
//				}
//			}
//		}
	}
	
	public static void setDataFromMap(Sheet sheet,Collection<Map<String,Object>> datas,Collection<SheetConfig>
		menuConfigs,SimpleDateFormat sdf){
//		if(datas!=null && !datas.isEmpty()){
//			ParseResult parseResult=parseMenu(sheet, menuConfigs);
//			for(Menu menu:parseResult.getFixedMenus()){
//				Cell dataCell=menu.getDataCell(1);
//				setCellValue(dataCell,datas.iterator().next().get(menu.getMenuConfig().getFieldName()),sdf);
//			}
//			int i=1;
//			for(Map<String,Object> data:datas){
//				for(Menu menu:parseResult.getNoFixedMenus()){
//					CellStyle cellStyle=menu.getDataCell(1).getCellStyle();
//					Cell dataCell=menu.getDataCell(i);
//					dataCell.setCellStyle(cellStyle);
//					setCellValue(dataCell,data.get(menu.getCheckMenuConfig().getFieldName()),sdf);
//				}
//				i++;
//			}
//		}
	}
	
	public static void setCellValue(Cell cell,Object value,SimpleDateFormat sdf){
		if(value!=null){
			if(value instanceof Date && sdf!=null){
				cell.setCellValue(sdf.format(value));
			}else if(value instanceof Number){
				cell.setCellValue(((Number)value).doubleValue());
			}else{
				cell.setCellValue(value.toString());
			}
		}
	}
	
	public static int getCellLeftColumnIndex(Cell cell){
		CellRangeAddress cellRangeAddress=getMergedRegion(cell);
		int columnIndex=cellRangeAddress==null?cell.getColumnIndex():cellRangeAddress.getFirstColumn();
		return columnIndex-1;
	}
	
	public static int getCellRightColumnIndex(Cell cell){
		CellRangeAddress cellRangeAddress=getMergedRegion(cell);
		int columnIndex=cellRangeAddress==null?cell.getColumnIndex():cellRangeAddress.getLastColumn();
		return columnIndex+1;
	}
	
	public static int getCellTopRowIndex(Cell cell){
		CellRangeAddress cellRangeAddress=getMergedRegion(cell);
		int rowIndex=cellRangeAddress==null?cell.getRowIndex():cellRangeAddress.getFirstRow();
		return rowIndex-1;
	}
	
	public static int getCellBottomRowIndex(Cell cell){
		CellRangeAddress cellRangeAddress=getMergedRegion(cell);
		int rowIndex=cellRangeAddress==null?cell.getRowIndex():cellRangeAddress.getLastRow();
		return rowIndex+1;
	}
	
	public static void moveCell(Collection<Cell> cells,int rowMoveNumber,int columnMoveNumber){
		Set<CellRangeAddress> addresses=Sets.newHashSet();
		for(Cell cell:cells){
			CellRangeAddress address=getMergedRegion(cell);
			if(address!=null){
				addresses.add(address);
			}else{
				moveCell(cell,rowMoveNumber,columnMoveNumber);
			}
		}
		Sheet sheet=cells.iterator().next().getSheet();
		for(CellRangeAddress address:addresses){
			moveCell(sheet,address,rowMoveNumber,columnMoveNumber);
		}
	}
	
	public static void moveCell(Cell cell,int rowMoveNumber,int columnMoveNumber){
		int rowIndex=cell.getRowIndex();
		int columnIndex=cell.getColumnIndex();
		Cell targetCell=getCellByIndex(cell.getSheet(),rowIndex+rowMoveNumber,columnIndex+columnMoveNumber);
		removeCell(targetCell);
		targetCell=getCellByIndex(cell.getSheet(),rowIndex+rowMoveNumber,columnIndex+columnMoveNumber);
		copyCell(cell,targetCell,true);
		removeCell(cell);
	}
	
	public static void moveCell(Sheet sheet,CellRangeAddress address,int rowMoveNumber,int columnMoveNumber){
		int firstRow=address.getFirstRow();
		int lastRow=address.getLastRow();
		int firstColumn=address.getFirstColumn();
		int lastColumn=address.getLastColumn();
		for(int row=firstRow;row<=lastRow;row++){
			for(int column=firstColumn;column<=lastColumn;column++){
				Cell moveCell=getCellByIndex(sheet,row,column);
				Cell targetCell=getCellByIndex(sheet,row+rowMoveNumber,column+columnMoveNumber);      
				removeCell(targetCell);
				targetCell=getCellByIndex(sheet,row+rowMoveNumber,column+columnMoveNumber);
				copyCell(moveCell,targetCell,true);
				removeCell(moveCell);
			}
		}
		
		CellRangeAddress targetAddress=new CellRangeAddress(firstRow+rowMoveNumber,lastRow+rowMoveNumber,
				firstColumn+columnMoveNumber,lastColumn+columnMoveNumber);
		sheet.addMergedRegion(targetAddress);
	}
	
	public static void removeCell(Cell cell){
		CellRangeAddress address=getMergedRegion(cell);
		if(address!=null){
			removeCell(cell.getSheet(),address);
		}else{
			cell.getRow().removeCell(getCellByIndex(cell.getSheet(),cell.getRowIndex(),cell.getColumnIndex()));
		}
	}
	
	public static void removeCell(Sheet sheet,CellRangeAddress address){
		int sheetMergeCount = sheet.getNumMergedRegions();  
		for (int i = sheetMergeCount-1; i >= 0; i--){
			CellRangeAddress range = sheet.getMergedRegion(i);  
			if(range.getFirstRow()==address.getFirstRow() && range.getLastRow()==address.getLastRow() && 
					range.getFirstColumn()==address.getFirstColumn() && range.getLastColumn()==address.getLastColumn()){  
				sheet.removeMergedRegion(i);
				for(int rowIndex=address.getFirstRow();rowIndex<=address.getLastRow();rowIndex++){
					for(int columnIndex=address.getFirstColumn();columnIndex<=address.getLastColumn();columnIndex++){
						removeCell(getCellByIndex(sheet,rowIndex,columnIndex));
					}
				}
			} 
		}
	}
	
	//TODO 改成children?或者直接删除?
//	public static List<String> getSubordinateMenus(Sheet sheet,String menuName){
//		List<String> menus=Lists.newArrayList();
//		Cell menu=getCellByMenuName(sheet,menuName);
//		if(menu!=null){
//			CellRangeAddress cellRangeAddress=getMergedRegion(menu);
//			if(cellRangeAddress==null){
//				return null;
//			}
//			int startColumn=cellRangeAddress.getFirstColumn();
//			int endColumn=cellRangeAddress.getLastColumn();
//			for(int column=startColumn;column<=endColumn;){
//				Cell subordinateMenu=getCellByIndex(sheet,getCellBottomRowIndex(menu),column);
//				menus.add(getStringValue(subordinateMenu));
//				column=getCellRightColumnIndex(subordinateMenu);
//			}
//		}
//		return menus;
//	}
	
	public static void setCellStyle(Cell cell,CellStyle style){
		CellRangeAddress address=getMergedRegion(cell);
		if(address==null){
			cell.setCellStyle(style);
		}else{
			for(int rowIndex=address.getFirstRow();rowIndex<=address.getLastRow();rowIndex++){
				for(int columnIndex=address.getFirstColumn();columnIndex<=address.getLastColumn();columnIndex++){
					Cell c=getCellByIndex(cell.getSheet(), rowIndex, columnIndex);
					c.setCellStyle(style);
				}
			}
		}
	}
	public static void copyCell(Cell srcCell, Cell targetCell,  
            boolean copyValueFlag) {  
        //样式  
        targetCell.setCellStyle(srcCell.getCellStyle());
        //评论  
        if (srcCell.getCellComment() != null) {  
            targetCell.setCellComment(srcCell.getCellComment());  
        }  
        // 不同数据类型处理  
        CellType srcCellType = srcCell.getCellTypeEnum();
        targetCell.setCellType(srcCellType);  
        if (copyValueFlag) {  
            if (srcCellType == CellType.NUMERIC) {  
                if (DateUtil.isCellDateFormatted(srcCell)) {  
                    targetCell.setCellValue(srcCell.getDateCellValue());  
                } else {  
                    targetCell.setCellValue(srcCell.getNumericCellValue());  
                }  
            } else if (srcCellType == CellType.STRING) {  
                targetCell.setCellValue(srcCell.getRichStringCellValue());  
            } else if (srcCellType == CellType.BLANK) {  
                // nothing21  
            } else if (srcCellType == CellType.BOOLEAN) {  
                targetCell.setCellValue(srcCell.getBooleanCellValue());  
            } else if (srcCellType == CellType.ERROR) {  
                targetCell.setCellErrorValue(srcCell.getErrorCellValue());  
            } else if (srcCellType == CellType.FORMULA) {  
                targetCell.setCellFormula(srcCell.getCellFormula());  
            } else { // nothing29  
            }  
        }  
    } 
	public static boolean isDateCell(Cell cell){
		if(cell==null || CellType.BLANK==cell.getCellTypeEnum()){
			return Boolean.TRUE;
		}
		if(CellType.NUMERIC==cell.getCellTypeEnum()){
			return DateUtil.isCellDateFormatted(cell);
		}
		if(CellType.STRING==cell.getCellTypeEnum()){
			String dateString=ExcelUtil.getStringValue(cell);
			if(StringUtils.isBlank(dateString)){
				return Boolean.TRUE;
			}else{
				return dateString.matches(RPT1_REGISTER_TIME);
			}
		}
		return Boolean.FALSE;
	}
	public static Date getDate(Cell cell){
		if(cell==null || CellType.BLANK==cell.getCellTypeEnum()){
			return null;
		}
		if(CellType.NUMERIC==cell.getCellTypeEnum() && DateUtil.isCellDateFormatted(cell)){
			return DateUtil.getJavaDate(cell.getNumericCellValue());
		}
		if(CellType.STRING==cell.getCellTypeEnum()){
			String dateString=ExcelUtil.getStringValue(cell);
			try {
				if(dateString.contains("/")){
					return SDF1.parse(dateString);
				}
				if(dateString.contains("-")){
					return SDF2.parse(dateString);
				}
			} catch (ParseException e) {
				e.printStackTrace();
			}
		}
		return null;
	}
	
	public static Cell getFirstCell(Sheet sheet,CellRangeAddress mergedRegion){
		if(sheet==null || mergedRegion==null){
			return null;
		}
		return getCellByIndex(sheet, mergedRegion.getFirstRow(), mergedRegion.getFirstColumn());
	}
	
	public static Cell getFirstCell(Cell cell){
		if(isMergedRegion(cell)){
			return getFirstCell(cell.getSheet(),getMergedRegion(cell));
		}
		return cell;
	}
	
	public static Object getValue(Cell cell){
		if(cell==null){
			return null;
		}
		CellType cellType=cell.getCellTypeEnum();
		if(CellType.FORMULA==cellType){
			cellType=cell.getCachedFormulaResultTypeEnum();
		}
		
		Object value=null;
		switch(cellType){
		case NUMERIC:
			if(DateUtil.isCellDateFormatted(cell)){
				value=cell.getDateCellValue();
			}else{
				value=cell.getNumericCellValue();
			}
			break;
		case STRING:value=cell.getStringCellValue();break;
		case BOOLEAN:value=cell.getBooleanCellValue();break;
		//TODO 由于getErrorCellValue()方法获得的是excel中的数字类型的错误码,似乎没有实际意义,暂定仍返回null
		case ERROR:
		default:
			throw new IllegalArgumentException("the CellType "+cellType+" of "+cell+" is not support");
		}
		return value;
	}
	
	/**
	 * 获取单元格内的内容，并以字符串的形式返回
	 * @author caotc
	 * @date 2016.4.24
	 * @param cell 单元格
	 * @return 单元格内容的字符串
	 */
	public static String getStringValue(Cell cell) {
		Object value=getValue(cell);
		String result=null;
		if(value!=null){
			if(value instanceof Date){
				result=DEFAULT_DATE_FORMAT.format(value);
			}
			if(value instanceof Double || value instanceof Boolean){
				result=String.valueOf(value);
			}
			if(value instanceof String){
				result=(String) value;
			}
		}
		return result;
	}

	public static void removeMergedRegion(Sheet sheet,CellRangeAddress cellAddress) {  
		int sheetMergeCount = sheet.getNumMergedRegions();
		for (int i = 0; i < sheetMergeCount; i++) {  
			CellRangeAddress range = sheet.getMergedRegion(i);  
			int firstColumnIndex = range.getFirstColumn();  
			int lastColumnIndex = range.getLastColumn();  
			int firstRowIndex = range.getFirstRow();  
			int lastRowIndex = range.getLastRow();  
			if(cellAddress!=null){
				if(cellAddress.getFirstColumn()==firstColumnIndex
				   &&cellAddress.getLastColumn()==lastColumnIndex
				   &&cellAddress.getFirstRow()==firstRowIndex
				   &&cellAddress.getLastRow()==lastRowIndex){
					sheet.removeMergedRegion(i);
				}
			} 
		}  
	}  
	
	/**
	* 用于将Excel表格中列号字母转成列索引，从1对应A开始
	* 
	* @param column列号
	* @return 列索引
	*/
	public static int columnToIndex(String column) {
		if (!column.matches("[A-Z]+")) {
			try {
				throw new Exception("Invalid parameter");
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		int index = 0;
		char[] chars = column.toUpperCase().toCharArray();
		for (int i = 0; i < chars.length; i++) {
			index += ((int) chars[i] - (int) 'A' + 1)* (int) Math.pow(26, chars.length - i - 1);
		}
		return index;
	}
	
	/**
	* 用于将excel表格中列索引转成列号字母，从A对应1开始
	* 
	* @param index
	*            列索引
	* @return 列号
	*/
	public static String indexToColumn(int index) {
		if (index <= 0) {
			try {
				throw new Exception("Invalid parameter");
			} catch (Exception e) {
				e.printStackTrace();                
			}         
		}         
		index--;         
		String column = "";         
		do {
			if (column.length() > 0) {
				index--;
			}
			column = ((char) (index % 26 + (int) 'A')) + column;
			index = (int) ((index - index % 26) / 26);
		} while (index > 0);
		return column;
	}
}
