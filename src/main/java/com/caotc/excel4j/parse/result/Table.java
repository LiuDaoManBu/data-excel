package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.parse.error.TableError;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.Collections2;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class Table {
  public static class Builder {
    private TableConfig tableConfig;
    private List<TableError> errors = Lists.newArrayList();;
    private SheetParseResult sheetParseResult;
    private Collection<Menu> menus;
    private Data fiexdData;
    private Collection<Data> noFiexdDatas = Lists.newArrayList();

    public Table build() {
      return new Table(this);
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public Builder setTableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public List<TableError> getErrors() {
      return errors;
    }

    public Builder setErrors(List<TableError> errors) {
      this.errors = errors;
      return this;
    }

    public SheetParseResult getSheetParseResult() {
      return sheetParseResult;
    }

    public Builder setSheetParseResult(SheetParseResult sheetParseResult) {
      this.sheetParseResult = sheetParseResult;
      return this;
    }

    public Collection<Menu> getMenus() {
      return menus;
    }

    public Builder setMenus(Collection<Menu> menus) {
      this.menus = menus;
      return this;
    }

    public Data getFiexdData() {
      return fiexdData;
    }

    public Builder setFiexdData(Data fiexdData) {
      this.fiexdData = fiexdData;
      return this;
    }

    public Collection<Data> getNoFiexdDatas() {
      return noFiexdDatas;
    }

    public Builder setNoFiexdDatas(Collection<Data> noFiexdDatas) {
      this.noFiexdDatas = noFiexdDatas;
      return this;
    }
    
  }

  public static Builder builder() {
    return new Builder();
  }

  private final TableConfig tableConfig;
  private final List<TableError> errors;
  private final SheetParseResult sheetParseResult;
  private final Collection<Menu> menus;
  private final Collection<Menu> dataMenus;
  private final Collection<Menu> fixedDataMenus;
  private final Collection<Menu> unFixedDataMenus;
  private final Collection<Menu> mixedDataMenus;
  private final Collection<Menu> mustMenus;
  private final Collection<Menu> notMustMenus;
  private final Data fiexdData;
  private final Collection<Data> noFiexdDatas;

  public Table(Builder builder) {
    this.tableConfig = builder.tableConfig;
    this.errors = builder.errors;
    this.sheetParseResult = builder.sheetParseResult;
    this.menus = builder.menus;
    this.fiexdData = builder.fiexdData;
    this.noFiexdDatas = builder.noFiexdDatas;

    dataMenus = Collections2.filter(menus, Menu::isDataMenu);
    fixedDataMenus = Collections2.filter(dataMenus, Menu::isFixedDataMenu);
    unFixedDataMenus = Collections2.filter(dataMenus, Menu::isUnFixedDataMenu);
    mixedDataMenus = Collections2.filter(dataMenus, Menu::isMixedDataMenu);
    mustMenus = Collections2.filter(menus, Menu::isMustMenu);
    notMustMenus = Collections2.filter(menus, Menu::isNotMustMenu);
  }

  public void loadTopMenus() {
    Collection<MenuConfig> menuConfigs = getTableConfig().getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    for (int rowIndex = sheet.getFirstRowNum(); rowIndex <= sheet.getLastRowNum(); rowIndex++) {
      Row row = sheet.getRow(rowIndex);
      for (int columnIndex = row.getFirstCellNum(); columnIndex < row
          .getLastCellNum(); columnIndex++) {
        StandardCell cell =
            StandardCell.valueOf(ExcelUtil.getCellByIndex(sheet, rowIndex, columnIndex));
        Collection<MenuConfig> config = Collections2.filter(menuConfigs,
            menuConfig -> menuConfig.getMenuMatcher().matches(cell));
        if (!CollectionUtils.isEmpty(config)) {
          menus.add(Menu.builder().setCell(cell).setMenuConfig(Iterables.getOnlyElement(config))
              .setTable(this).build());
        }
      }
    }
  }

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

  public void findMenus() {
    loadTopMenus();
    findChildrenMenus();
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

  public List<TableError> getErrors() {
    return errors;
  }

  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }

}
