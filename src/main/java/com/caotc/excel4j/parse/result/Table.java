package com.caotc.excel4j.parse.result;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Row.MissingCellPolicy;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.parse.error.TableError;
import com.google.common.base.Preconditions;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

public class Table {
  public static class Builder {
    private TableConfig tableConfig;
    private SheetParseResult sheetParseResult;
    private List<TableError> errors;

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

    public List<TableError> getErrors() {
      return errors;
    }

    public Builder setErrors(List<TableError> errors) {
      this.errors = errors;
      return this;
    }
  }

  private static final Traverser<Menu<?>> MENU_TRAVERSER =
      Traverser.forTree(new SuccessorsFunction<Menu<?>>() {
        @Override
        public Iterable<? extends Menu<?>> successors(Menu<?> node) {
          return node.getChildrenMenus();
        }
      });

  private static final Function<MenuConfig<?>, String> MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION =
      config -> config + "don't have any matches cell";

  public static Builder builder() {
    return new Builder();
  }

  private final TableConfig tableConfig;
  private final ImmutableList<TableError> errors;
  private final SheetParseResult sheetParseResult;
  private final ImmutableCollection<Menu<?>> topMenus;

  // private final ImmutableCollection<Menu> menus;
  // private final ImmutableCollection<Menu> dataMenus;
  // private final ImmutableCollection<Menu> fixedDataMenus;
  // private final ImmutableCollection<Menu> unFixedDataMenus;
  // private final ImmutableCollection<Menu> mixedDataMenus;
  // private final ImmutableCollection<Menu> mustMenus;
  // private final ImmutableCollection<Menu> notMustMenus;

  public Table(Builder builder) {
    tableConfig = builder.tableConfig;
    sheetParseResult = builder.sheetParseResult;
    topMenus =
        loadTopMenus().stream().map(Menu.Builder::build).collect(ImmutableSet.toImmutableSet());
    // dataMenus = Collections2.filter(menus, Menu::isDataMenu);
    // fixedDataMenus = Collections2.filter(dataMenus, Menu::isFixedDataMenu);
    // unFixedDataMenus = Collections2.filter(dataMenus, Menu::isUnFixedDataMenu);
    // mixedDataMenus = Collections2.filter(dataMenus, Menu::isMixedDataMenu);
    // mustMenus = Collections2.filter(menus, Menu::isMustMenu);
    // notMustMenus = Collections2.filter(menus, Menu::isNotMustMenu);

    // new TableError(this, tableConfig.getMatcher().getMessageFunction().apply(this));
    // TODO 顺序问题?
    ImmutableCollection<MenuConfig<?>> matchesMenuConfigs =
        topMenus.stream().map(Menu::getMenuConfig).collect(ImmutableSet.toImmutableSet());

    errors = tableConfig.getTopMenuConfigs().stream()
        .filter(config -> !matchesMenuConfigs.contains(config))
        .map(config -> new TableError(this, MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION.apply(config)))
        .collect(ImmutableList.toImmutableList());
  }

  private ImmutableCollection<Menu.Builder<?>> loadTopMenus() {
    com.google.common.collect.ImmutableSet.Builder<Menu.Builder<?>> builder =
        ImmutableSet.builder();

    ImmutableCollection<MenuConfig<?>> menuConfigs = tableConfig.getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    // TODO 改写stream,效率优化
    for (int rowIndex = sheet.getFirstRowNum(); rowIndex <= sheet.getLastRowNum(); rowIndex++) {
      Row row = sheet.getRow(rowIndex);
      for (int columnIndex = row.getFirstCellNum(); columnIndex < row
          .getLastCellNum(); columnIndex++) {
        // TODO 获取cell模式正确?
        StandardCell cell =
            StandardCell.valueOf(row.getCell(columnIndex, MissingCellPolicy.CREATE_NULL_AS_BLANK));
        // TODO 重复匹配问题
        Optional<MenuConfig<?>> optional = menuConfigs.stream()
            .filter(menuConfig -> menuConfig.getMenuMatcher().test(cell)).findAny();
        if (optional.isPresent()) {
          // TODO safe
          Menu.Builder topMenuBuilder = Menu.builder();
          builder.add(topMenuBuilder.setCell(cell).setMenuConfig(optional.get()).setTable(this));
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
    return getMenus().stream().filter(menu -> menu.getName().equals(menuName)).findAny();
  }

  public ImmutableList<Menu<?>> getMenus() {
    return topMenus.stream().map(MENU_TRAVERSER::breadthFirst).flatMap(Streams::stream)
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Menu<?>> getDataMenus() {
    return getMenus().stream().filter(Menu::isDataMenu).collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Menu<?>> getFixedDataMenus() {
    return getDataMenus().stream().filter(Menu::isFixedDataMenu)
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Menu<?>> getUnFixedDataMenus() {
    return getDataMenus().stream().filter(Menu::isUnFixedDataMenu)
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Menu<?>> getMixedDataMenus() {
    return getDataMenus().stream().filter(Menu::isMixedDataMenu)
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Menu<?>> getMustMenus() {
    return getMenus().stream().filter(Menu::isMustMenu).collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Menu<?>> getNotMustMenus() {
    return getMenus().stream().filter(Menu::isNotMustMenu).collect(ImmutableList.toImmutableList());
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ImmutableList<TableError> getErrors() {
    return errors;
  }

  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }

  public ImmutableCollection<Menu<?>> getTopMenus() {
    return topMenus;
  }

}
