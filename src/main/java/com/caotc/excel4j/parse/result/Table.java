package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.parse.error.TableError;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;

public class Table {
  private TableConfig tableConfig;
  private List<TableError> errors;
  private SheetParseResult sheetParseResult;
  private Collection<Menu> menus;
  private final Collection<Menu> dataMenus = Collections2.filter(menus, new Predicate<Menu>() {
    @Override
    public boolean apply(Menu input) {
      return MenuType.DATA_MENU.equals(input.getMenuType());
    }
  });
  private final Collection<Menu> fixedDataMenus = Collections2.filter(dataMenus, new Predicate<Menu>() {
    @Override
    public boolean apply(Menu input) {
      return LoadType.FIXED.equals(input.getCheckMenuConfig().getLoadType());
    }
  });
  private final Collection<Menu> unFixedDataMenus = Collections2.filter(dataMenus, new Predicate<Menu>() {
    @Override
    public boolean apply(Menu input) {
      return LoadType.UNFIXED.equals(input.getCheckMenuConfig().getLoadType());
    }
  });
  private final Collection<Menu> mixedDataMenus = Collections2.filter(dataMenus, new Predicate<Menu>() {
    @Override
    public boolean apply(Menu input) {
      return LoadType.MIXED.equals(input.getCheckMenuConfig().getLoadType());
    }
  });
  private final Collection<Menu> mustMenus = Collections2.filter(menus, new Predicate<Menu>() {
    @Override
    public boolean apply(Menu input) {
      return MenuNecessity.MUST.equals(input.getCheckMenuConfig().getMenuNecessity());
    }
  });
  private final Collection<Menu> noMustMenus = Collections2.filter(menus, new Predicate<Menu>() {
    @Override
    public boolean apply(Menu input) {
      return MenuNecessity.NOT_MUST.equals(input.getCheckMenuConfig().getMenuNecessity());
    }
  });
  
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
