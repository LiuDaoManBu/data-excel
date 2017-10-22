package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import com.alibaba.fastjson.JSONArray;
import com.caotc.excel4j.collect.ImmutableTree;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.parse.error.TableError;
import com.google.common.base.Optional;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

public class Table {
  public static class Builder {
    private TableConfig tableConfig;
    private SheetParseResult sheetParseResult;

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

    public SheetParseResult getSheetParseResult() {
      return sheetParseResult;
    }

    public Builder setSheetParseResult(SheetParseResult sheetParseResult) {
      this.sheetParseResult = sheetParseResult;
      return this;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final TableConfig tableConfig;
  private final List<TableError> errors;
  private final SheetParseResult sheetParseResult;
  private final ImmutableCollection<ImmutableTree<Menu>> menuTrees;
  // private final ImmutableCollection<Menu> menus;
  // private final ImmutableCollection<Menu> dataMenus;
  // private final ImmutableCollection<Menu> fixedDataMenus;
  // private final ImmutableCollection<Menu> unFixedDataMenus;
  // private final ImmutableCollection<Menu> mixedDataMenus;
  // private final ImmutableCollection<Menu> mustMenus;
  // private final ImmutableCollection<Menu> notMustMenus;
  private final Data data;

  public Table(Builder builder) {
    tableConfig = builder.tableConfig;
    // TODO
    errors = null;
    sheetParseResult = builder.sheetParseResult;
    menuTrees = FluentIterable.from(loadTopMenus())
        .transform(menu -> ImmutableTree.using(menu, Menu::getChildrenMenus)).toSet();

    // dataMenus = Collections2.filter(menus, Menu::isDataMenu);
    // fixedDataMenus = Collections2.filter(dataMenus, Menu::isFixedDataMenu);
    // unFixedDataMenus = Collections2.filter(dataMenus, Menu::isUnFixedDataMenu);
    // mixedDataMenus = Collections2.filter(dataMenus, Menu::isMixedDataMenu);
    // mustMenus = Collections2.filter(menus, Menu::isMustMenu);
    // notMustMenus = Collections2.filter(menus, Menu::isNotMustMenu);

    com.google.common.collect.ImmutableListMultimap.Builder<Menu, StandardCell> dataBuilder =
        ImmutableListMultimap.builder();
    getDataMenus().forEach(menu -> dataBuilder.putAll(menu,
        menu.getCheckMenuConfig().getMenuLoadConfig().getLoadType().getDataCells(menu)));
    data = new Data(this, dataBuilder.build());
  }

  private ImmutableCollection<Menu> loadTopMenus() {
    com.google.common.collect.ImmutableSet.Builder<Menu> builder = ImmutableSet.builder();

    Collection<MenuConfig> menuConfigs = getTableConfig().getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    for (int rowIndex = sheet.getFirstRowNum(); rowIndex <= sheet.getLastRowNum(); rowIndex++) {
      Row row = sheet.getRow(rowIndex);
      for (int columnIndex = row.getFirstCellNum(); columnIndex < row
          .getLastCellNum(); columnIndex++) {
        StandardCell cell =
            StandardCell.valueOf(row.getCell(columnIndex, MissingCellPolicy.CREATE_NULL_AS_BLANK));
        Optional<MenuConfig> optional =
            Iterables.tryFind(menuConfigs, menuConfig -> menuConfig.getMenuMatcher().matches(cell));
        if (optional.isPresent()) {
          builder.add(
              Menu.builder().setCell(cell).setMenuConfig(optional.get()).setTable(this).build());
        }
      }
    }
    return builder.build();
  }

  // TODO
  public JSONArray getDatas() {
    JSONArray datas = new JSONArray();
    // unFiexdDatas.stream().map(Data::getJsonData).peek(data ->
    // data.putAll(fiexdData.getJsonData()))
    // .forEach(datas::add);
    return datas;
  }

  public <T> List<T> getDatas(Class<T> clazz) {
    return getDatas().toJavaList(clazz);
  }

  public void checkMenus() {
    // TODO
    // Map<MenuConfig, Menu> menuConfigToMenus = Maps.newHashMap();
    // for (Menu menu : menus) {
    // if (menu.getMenuConfig() != null) {
    // menuConfigToMenus.put(menu.getMenuConfig(), menu);
    // }
    // }
    // for(MenuConfig menuConfig:menuConfigs){
    // if(!menuConfigToMenus.containsKey(menuConfig) && menuConfig.getMustFlag()){
    // addError("请检查模板是否有误,工作簿"+sheet.getSheetName()+"未找到菜单:"+menuConfig.getMenuNameMatcher().getMatchString());
    // }
    // }
  }

  public Optional<Menu> getMenu(String menuName) {
    return Iterables.tryFind(getMenus(), menu -> menu.getName().equals(menuName));
  }

  public Iterable<Menu> getTopMenus() {
    return Iterables.transform(menuTrees, ImmutableTree::getRoot);
  }
  
  public FluentIterable<Menu> getMenus() {
    return FluentIterable.from(menuTrees).transformAndConcat(ImmutableTree::children);
  }

  public FluentIterable<Menu> getDataMenus() {
    return getMenus().filter(Menu::isDataMenu);
  }

  public FluentIterable<Menu> getFixedDataMenus() {
    return getDataMenus().filter(Menu::isFixedDataMenu);
  }

  public FluentIterable<Menu> getUnFixedDataMenus() {
    return getDataMenus().filter(Menu::isUnFixedDataMenu);
  }

  public FluentIterable<Menu> getMixedDataMenus() {
    return getDataMenus().filter(Menu::isMixedDataMenu);
  }

  public FluentIterable<Menu> getMustMenus() {
    return getMenus().filter(Menu::isMustMenu);
  }

  public FluentIterable<Menu> getNotMustMenus() {
    return getMenus().filter(Menu::isNotMustMenu);
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

  public Data getData() {
    return data;
  }

  public ImmutableCollection<ImmutableTree<Menu>> getMenuTrees() {
    return menuTrees;
  }

}
