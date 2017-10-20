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
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class Table {
  public static class Builder {
    private TableConfig tableConfig;
    private List<TableError> errors;
    private SheetParseResult sheetParseResult;
    private Collection<Menu> menus;
    private Data data;

    public void parseFixedDatas() {
      Collections2.filter(menus, Menu::isDataMenu).forEach(menu->{
        
      });
      for (Menu menu : Collections2.filter(menus, Menu::isDataMenu)) {
        StandardCell dataCell = menu.nextDataCell(null).get();
      }
      
    }
    
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

    public Data getData() {
      return data;
    }

    public Builder setData(Data data) {
      this.data = data;
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
  private final Data data;

  public Table(Builder builder) {
    tableConfig = builder.tableConfig;
    errors = builder.errors;
    sheetParseResult = builder.sheetParseResult;
    menus = builder.menus;

    dataMenus = Collections2.filter(menus, Menu::isDataMenu);
    fixedDataMenus = Collections2.filter(dataMenus, Menu::isFixedDataMenu);
    unFixedDataMenus = Collections2.filter(dataMenus, Menu::isUnFixedDataMenu);
    mixedDataMenus = Collections2.filter(dataMenus, Menu::isMixedDataMenu);
    mustMenus = Collections2.filter(menus, Menu::isMustMenu);
    notMustMenus = Collections2.filter(menus, Menu::isNotMustMenu);
    
    parseDatas(builder);
    
    data = builder.data;
  }

  public void loadMenus() {
    loadTopMenus();
    menus.forEach(Menu::load);
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

  public JSONArray getDatas() {
    JSONArray datas = new JSONArray();
    unFiexdDatas.stream().map(Data::getJsonData).peek(data -> data.putAll(fiexdData.getJsonData()))
        .forEach(datas::add);
    return datas;
  }

  public <T> List<T> getDatas(Class<T> clazz) {
    return getDatas().toJavaList(clazz);
  }

  public void checkMenus() {
    //TODO
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

  public void parseUnFixedDatas() {
    if (!CollectionUtils.isEmpty(unFixedDataMenus)) {
      Menu firstNoFixedMenu = Iterables.getFirst(unFixedDataMenus,null);
      Cell currentFirstNoFixedMenuDataCell =
          firstNoFixedMenu.nextDataCell(firstNoFixedMenu.getCell());
       for(int
       i=1;ExcelUtil.isDataCell(currentFirstNoFixedMenuDataCell,firstNoFixedMenu,menus);i++){
       Map<Menu,Cell> menuToCells=Maps.newHashMap();
       for(Menu noFixedMenu:unFixedDataMenus){
       Cell dataCell=noFixedMenu.getDataCell(i);
       menuToCells.put(noFixedMenu,dataCell);
       }
       if(!CollectionUtils.isEmpty(menuToCells)){
       unFiexdDatas.add(new Data(menuToCells));
       }
       currentFirstNoFixedMenuDataCell=firstNoFixedMenu.nextDataCell(currentFirstNoFixedMenuDataCell);
       }
      for (Iterator<Data> iter = unFiexdDatas.iterator(); iter.hasNext();) {
        Data data = iter.next();
        if (data.getJsonData().isEmpty()) {
          iter.remove();
        } else {
          for (CellData cellData : data.getMenuToCells()) {
            Menu menu = cellData.getMenu();
            StandardCell dataCell = cellData.getValueCell();
            menu.checkDataCell(dataCell);
          }
        }
      }
    }
  }

  public void parseDatas(Builder builder) {
    parseFixedDatas(builder);
    parseUnFixedDatas();
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

  public Collection<Menu> getMenus() {
    return menus;
  }

  public Collection<Menu> getDataMenus() {
    return dataMenus;
  }

  public Collection<Menu> getFixedDataMenus() {
    return fixedDataMenus;
  }

  public Collection<Menu> getUnFixedDataMenus() {
    return unFixedDataMenus;
  }

  public Collection<Menu> getMixedDataMenus() {
    return mixedDataMenus;
  }

  public Collection<Menu> getMustMenus() {
    return mustMenus;
  }

  public Collection<Menu> getNotMustMenus() {
    return notMustMenus;
  }

  public Data getData() {
    return data;
  }


}
