package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.parse.error.TableError;
import com.google.common.base.Preconditions;
import com.google.common.collect.Collections2;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

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
  private final ImmutableCollection<Menu<?>> topMenus;
  private final Traverser<Menu<?>> menuTraverser;
  // private final ImmutableCollection<Menu> menus;
  // private final ImmutableCollection<Menu> dataMenus;
  // private final ImmutableCollection<Menu> fixedDataMenus;
  // private final ImmutableCollection<Menu> unFixedDataMenus;
  // private final ImmutableCollection<Menu> mixedDataMenus;
  // private final ImmutableCollection<Menu> mustMenus;
  // private final ImmutableCollection<Menu> notMustMenus;

  public Table(Builder builder) {
    tableConfig = builder.tableConfig;
    // TODO
    errors = null;
    sheetParseResult = builder.sheetParseResult;
    topMenus=loadTopMenus();
    menuTraverser=Traverser.forTree(new SuccessorsFunction<Menu<?>>() {
      @Override
      public Iterable<? extends Menu<?>> successors(Menu<?> node) {
        return node.getChildrenMenus();
      }
    });

    // dataMenus = Collections2.filter(menus, Menu::isDataMenu);
    // fixedDataMenus = Collections2.filter(dataMenus, Menu::isFixedDataMenu);
    // unFixedDataMenus = Collections2.filter(dataMenus, Menu::isUnFixedDataMenu);
    // mixedDataMenus = Collections2.filter(dataMenus, Menu::isMixedDataMenu);
    // mustMenus = Collections2.filter(menus, Menu::isMustMenu);
    // notMustMenus = Collections2.filter(menus, Menu::isNotMustMenu);
  }

  private ImmutableCollection<Menu<?>> loadTopMenus() {
    com.google.common.collect.ImmutableSet.Builder<Menu<?>> builder = ImmutableSet.builder();

    Collection<MenuConfig<?>> menuConfigs = tableConfig.getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    for (int rowIndex = sheet.getFirstRowNum(); rowIndex <= sheet.getLastRowNum(); rowIndex++) {
      Row row = sheet.getRow(rowIndex);
      for (int columnIndex = row.getFirstCellNum(); columnIndex < row
          .getLastCellNum(); columnIndex++) {
        StandardCell cell =
            StandardCell.valueOf(row.getCell(columnIndex, MissingCellPolicy.CREATE_NULL_AS_BLANK));
        com.google.common.base.Optional<MenuConfig<?>> optional =
            Iterables.tryFind(menuConfigs, menuConfig -> menuConfig.getMenuMatcher().matches(cell));
        if (optional.isPresent()) {
          com.caotc.excel4j.parse.result.Menu.Builder topMenuBuilder = Menu.builder();
          builder.add(
              topMenuBuilder.setCell(cell).setMenuConfig(optional.get()).setTable(this).build());
        }
      }
    }
    return builder.build();
  }

  public <T> T get(Class<T> type) {
    Optional<T> optional = tableConfig.getParserConfig().newInstance(type);
    Preconditions.checkArgument(optional.isPresent());
    topMenus.forEach(menu -> menu.getData().setFieldValue(optional.get()));
    return optional.get();
  }

  // TODO
  public ImmutableList<StandardCell> getCells(Menu<?> menu) {
    return menu.getData().getValueCells();
  }

  public ImmutableList<StandardCell> getCells(String menuName) {
    return getCells(getMenu(menuName).orElse(null));
  }

  public Optional<StandardCell> getCell(Menu<?> menu) {
    return Optional.ofNullable(Iterables.getOnlyElement(getCells(menu), null));
  }

  public Optional<StandardCell> getCell(String menuName) {
    return getCell(getMenu(menuName).orElse(null));
  }

  public ImmutableList<Object> getValues(Menu<?> menu) {
    return ImmutableList.copyOf(Collections2.transform(getCells(menu), StandardCell::getValue));
  }

  public ImmutableList<Object> getValues(String menuName) {
    return getValues(getMenu(menuName).orElse(null));
  }

  public <K> ImmutableList<K> getValues(Menu<?> menu, Class<K> type) {
    return ImmutableList
        .copyOf(Collections2.transform(getValues(menu), value -> menu.cast(value, type)));
  }

  public <K> ImmutableList<K> getValues(String menuName, Class<K> type) {
    return getValues(getMenu(menuName).orElse(null), type);
  }

  public Optional<Object> getValue(Menu<?> menu) {
    return getCell(menu).map(StandardCell::getValue);
  }

  public Optional<Object> getValue(String menuName) {
    return getValue(getMenu(menuName).orElse(null));
  }

  public <K> Optional<K> getValue(Menu<?> menu, Class<K> type) {
    return getValue(menu).map(value -> menu.cast(value, type));
  }

  public <K> Optional<K> getValue(String menuName, Class<K> type) {
    Menu<?> menu = getMenu(menuName).orElse(null);
    return getValue(menu).map(value -> menu.cast(value, type));
  }
  //

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

  public Optional<Menu<?>> getMenu(String menuName) {
    return Iterables.tryFind(getMenus(), menu -> menu.getName().equals(menuName)).toJavaUtil();
  }

  public FluentIterable<Menu<?>> getMenus() {
    return FluentIterable.from(topMenus).transformAndConcat(menuTraverser::breadthFirst);
  }

  public FluentIterable<Menu<?>> getDataMenus() {
    return getMenus().filter(Menu::isDataMenu);
  }

  public FluentIterable<Menu<?>> getFixedDataMenus() {
    return getDataMenus().filter(Menu::isFixedDataMenu);
  }

  public FluentIterable<Menu<?>> getUnFixedDataMenus() {
    return getDataMenus().filter(Menu::isUnFixedDataMenu);
  }

  public FluentIterable<Menu<?>> getMixedDataMenus() {
    return getDataMenus().filter(Menu::isMixedDataMenu);
  }

  public FluentIterable<Menu<?>> getMustMenus() {
    return getMenus().filter(Menu::isMustMenu);
  }

  public FluentIterable<Menu<?>> getNotMustMenus() {
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

  public ImmutableCollection<Menu<?>> getTopMenus() {
    return topMenus;
  }

}
