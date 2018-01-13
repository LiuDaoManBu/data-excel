package com.caotc.excel4j.parse.result;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.config.TableConfig;
import com.caotc.excel4j.parse.error.TableError;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
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

  public Table(Builder builder) {
    tableConfig = builder.tableConfig;
    sheetParseResult = builder.sheetParseResult;
    topMenus = loadTopMenus().map(Menu.Builder::build).collect(ImmutableSet.toImmutableSet());

    // new TableError(this, tableConfig.getMatcher().getMessageFunction().apply(this));
    // TODO 顺序问题?
    ImmutableCollection<MenuConfig<?>> matchesMenuConfigs =
        topMenus.stream().map(Menu::getMenuConfig).collect(ImmutableSet.toImmutableSet());

    errors = tableConfig.getTopMenuConfigs().stream()
        .filter(config -> !matchesMenuConfigs.contains(config))
        .map(config -> new TableError(this, MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION.apply(config)))
        .collect(ImmutableList.toImmutableList());
  }

  private Stream<Menu.Builder<?>> loadTopMenus() {
    ImmutableCollection<MenuConfig<?>> menuConfigs = tableConfig.getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    return ExcelUtil.getCells(sheet).map(StandardCell::valueOf).map(cell -> {
      // TODO 重复匹配问题
      Optional<MenuConfig<?>> optional = menuConfigs.stream()
          .filter(menuConfig -> menuConfig.getMenuMatcher().test(cell)).findAny();
      // TODO safe
      return optional.map(t -> new Menu.Builder().setCell(cell).setMenuConfig(t).setTable(this));
    }).filter(Optional::isPresent).map(Optional::get);
  }

  public <T> T get(Class<T> type) {
    Optional<T> optional = tableConfig.getEffectiveParserConfig().newInstance(type);
    Preconditions.checkArgument(optional.isPresent());
    topMenus.forEach(menu -> menu.getData().setFieldValue(optional.get()));
    return optional.get();
  }

  public Optional<Menu<?>> findMenu(String menuName) {
    return getMenus().filter(menu -> menu.getName().equals(menuName)).findAny();
  }

  public Stream<Menu<?>> getMenus() {
    return topMenus.stream().map(MENU_TRAVERSER::breadthFirst).flatMap(Streams::stream);
  }

  public Stream<Menu<?>> getDataMenus() {
    return getMenus().filter(Menu::isDataMenu);
  }

  public Stream<Menu<?>> getFixedDataMenus() {
    return getDataMenus().filter(Menu::isFixedDataMenu);
  }

  public Stream<Menu<?>> getUnFixedDataMenus() {
    return getDataMenus().filter(Menu::isUnFixedDataMenu);
  }

  public Stream<Menu<?>> getMustMenus() {
    return getMenus().filter(Menu::isMustMenu);
  }

  public Stream<Menu<?>> getNotMustMenus() {
    return getMenus().filter(Menu::isNotMustMenu);
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
