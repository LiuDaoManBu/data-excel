package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.parse.error.TableError;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class Table {
  private TableConfig tableConfig;
  private List<TableError> errors;
  private SheetParseResult sheetParseResult;
  private Collection<Menu> menus;
  private final Collection<Menu> dataMenus = Collections2.filter(menus,Menu::isDataMenu);
  private final Collection<Menu> fixedDataMenus = Collections2.filter(dataMenus,Menu::isFixedDataMenu);
  private final Collection<Menu> unFixedDataMenus = Collections2.filter(dataMenus,Menu::isUnFixedDataMenu);
  private final Collection<Menu> mixedDataMenus = Collections2.filter(dataMenus, Menu::isMixedDataMenu);
  private final Collection<Menu> mustMenus = Collections2.filter(menus,Menu::isMustMenu);
  private final Collection<Menu> noMustMenus = Collections2.filter(menus, Menu::isNotMustMenu);
  private Data fiexdData;
  private Collection<Data> noFiexdDatas = Lists.newArrayList();
  
  public JSONArray getJsonDatas() {
    JSONArray array = new JSONArray();
    for (Data data : datas) {
      array.add(data.getJsonData());
    }
    return array;
  }
  
  public <T> Collection<T> getClassDatas(Collection<T> collection, Class<T> clazz) {
    for (Data data : datas) {
      collection.add(JSONObject.toJavaObject(data.getJsonData(), clazz));
    }
    return collection;
  }
  
  public boolean addMenu(Menu menu) {
    boolean result = Boolean.FALSE;
    if (!menus.contains(menu)) {
      result = menus.add(menu);
      if (menu.getMenuConfig() != null && menu.getMenuConfig().getFieldName() != null) {
        fieldNameToMenus.put(menu.getMenuConfig().getFieldName(), menu);
      }
      MenuConfig menuConfig = menu.getCheckMenuConfig();
      if (menuConfig.getData() || menu.getMenuConfig() == null) {
        if (menuConfig.getSingleData()) {
          fixedMenus.add(menu);
        } else {
          noFixedMenus.add(menu);
        }
        if (menuConfig.getDynamic()) {
          dynamicMenus.add(menu);
        }
      }
    }
    return result;
  }

  public Menu addMenu(StandardCell cell) {
    Menu menu = new Menu(cell);
    addMenu(menu);
    return menu;
  }

  public Menu addMenu(StandardCell cell, MenuConfig menuConfig) {
    Menu menu = new Menu(cell, menuConfig);
    addMenu(menu);
    return menu;
  }

  public void findMenus() {
    loadTopMenus();
    findChildrenMenus();
  }

  public void loadTopMenus() {
    for (int rowIndex = sheet.getFirstRowNum(); rowIndex <= sheet.getLastRowNum(); rowIndex++) {
      Row row = sheet.getRow(rowIndex);
      for (int columnIndex = row.getFirstCellNum(); columnIndex < row
          .getLastCellNum(); columnIndex++) {
        Cell cell = row.getCell(columnIndex);
        CellRangeAddress mergedRegion = ExcelUtil.getMergedRegion(cell);
        if (mergedRegion == null || (cell.getRowIndex() == mergedRegion.getFirstRow()
            && cell.getColumnIndex() == mergedRegion.getFirstColumn())) {
          String value = ExcelUtil.getStringValue(cell);
          // for(MenuConfig menuConfig:sheetConfig.menuConfigs){
          // if(menuConfig.getParentMenuConfig()==null &&
          // menuConfig.getMenuNameMatcher().matches(value)){
          // addMenu(cell, menuConfig);
          // break;
          // }
          // }
        }
      }
    }
  }

  public void findChildrenMenus() {
    List<Menu> childrenMenus = Lists.newArrayList();
    for (Menu menu : menus) {
      childrenMenus.addAll(menu.findChildrenMenus());
    }
    for (Menu childrenMenu : childrenMenus) {
      addMenu(childrenMenu);
    }
  }

  public void checkMenus() {
    Map<MenuConfig, Menu> menuConfigToMenus = Maps.newHashMap();
    for (Menu menu : menus) {
      if (menu.getMenuConfig() != null) {
        menuConfigToMenus.put(menu.getMenuConfig(), menu);
      }
    }
    // for(MenuConfig menuConfig:menuConfigs){
    // if(!menuConfigToMenus.containsKey(menuConfig) && menuConfig.getMustFlag()){
    // addError("请检查模板是否有误,工作簿"+sheet.getSheetName()+"未找到菜单:"+menuConfig.getMenuNameMatcher().getMatchString());
    // }
    // }
  }

  public void parseFixedMenuDatas() {
    Collection<CellData> cellDatas = Lists.newArrayList();
    for (Menu fixedMenu : fixedMenus) {
      Cell dataCell = fixedMenu.nextDataCell(fixedMenu.getCell());
      if (fixedMenu.hasCheckMenuConfig()) {
        fixedMenu.checkDataCell(dataCell);
      }
      if (StringUtils.isNotBlank(ExcelUtil.getStringValue(dataCell))) {
        CellData cellData = new CellData();
        cellData.setMenu(fixedMenu);
        cellData.setValueCell(dataCell);
        cellDatas.add(cellData);
      }
    }
    fiexdData = new Data(cellDatas);
  }

  public void parseNoFixedMenuDatas() {
    if (!CollectionUtils.isEmpty(noFixedMenus)) {
      Menu firstNoFixedMenu = noFixedMenus.iterator().next();
      Cell currentFirstNoFixedMenuDataCell =
          firstNoFixedMenu.nextDataCell(firstNoFixedMenu.getCell());
      // for(int
      // i=1;ExcelUtil.isDataCell(currentFirstNoFixedMenuDataCell,firstNoFixedMenu,menus);i++){
      // Map<Menu,Cell> menuToCells=Maps.newHashMap();
      // for(Menu noFixedMenu:noFixedMenus){
      // Cell dataCell=noFixedMenu.getDataCell(i);
      // menuToCells.put(noFixedMenu,dataCell);
      // }
      // if(!CollectionUtils.isEmpty(menuToCells)){
      // noFiexdDatas.add(new Data(menuToCells));
      // }
      // currentFirstNoFixedMenuDataCell=firstNoFixedMenu.nextDataCell(currentFirstNoFixedMenuDataCell);
      // }
      for (Iterator<Data> iter = noFiexdDatas.iterator(); iter.hasNext();) {
        Data data = iter.next();
        if (data.getJsonData().isEmpty()) {
          iter.remove();
        } else {
          for (CellData cellData : data.getMenuToCells()) {
            Menu menu = cellData.getMenu();
            Cell dataCell = cellData.getValueCell();
            if (menu.hasCheckMenuConfig()) {
              menu.checkDataCell(dataCell);
            }
          }
        }
      }
    }
  }

  public void parseDatas() {
    parseFixedMenuDatas();
    parseNoFixedMenuDatas();
    for (Data noFiexData : noFiexdDatas) {
      Collection<CellData> cellDatas = Lists.newLinkedList(noFiexData.getMenuToCells());
      cellDatas.addAll(fiexdData.getMenuToCells());
      datas.add(new Data(cellDatas));
    }
  }
  
  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public void setTableConfig(TableConfig tableConfig) {
    this.tableConfig = tableConfig;
  }

  public List<TableError> getErrors() {
    return errors;
  }

  public void setErrors(List<TableError> errors) {
    this.errors = errors;
  }

  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }

  public void setSheetParseResult(SheetParseResult sheetParseResult) {
    this.sheetParseResult = sheetParseResult;
  }

}
