package com.github.liudaomanbu.excel.parse.result;

import java.util.Collection;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import com.github.liudaomanbu.excel.config.MenuConfig;
import com.github.liudaomanbu.excel.config.TableConfig;
import com.github.liudaomanbu.excel.constant.Necessity;
import com.github.liudaomanbu.excel.parse.error.TableValidationError;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.github.liudaomanbu.excel.validator.BaseValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

public class Table {
  public static class Builder {
    private TableConfig config;
    private SheetParseResult sheetParseResult;

    public Table build() {
      return new Table(this);
    }

    public TableConfig getConfig() {
      return config;
    }

    public Builder setConfig(TableConfig config) {
      this.config = config;
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

  private final Traverser<Menu> MENU_TRAVERSER = Traverser.forTree(new SuccessorsFunction<Menu>() {
    @Override
    public Iterable<? extends Menu> successors(Menu node) {
      return node.getChildrens();
    }
  });

  private final Function<MenuConfig, String> MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION =
      config -> config + "don't have any matches cell";

  public static Builder builder() {
    return new Builder();
  }

  private final TableConfig config;
  private final ImmutableList<ValidationError<Table>> errors;
  private final SheetParseResult sheetParseResult;
  private final ImmutableCollection<Menu> topMenus;
  private final TableData data;

  public Table(Builder builder) {
    config = builder.config;
    sheetParseResult = builder.sheetParseResult;
    topMenus = loadTopMenus().map(Menu.Builder::build).collect(ImmutableSet.toImmutableSet());
    errors = createMenuConfigValidator().validate(this).stream()
        .collect(ImmutableList.toImmutableList());
    this.data = new TableData(this);
  }

  private Stream<Menu.Builder> loadTopMenus() {
    ImmutableCollection<MenuConfig> menuConfigs = config.getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    return ExcelUtil.getCells(sheet).map(StandardCell::valueOf).map(cell -> {
      Optional<MenuConfig> optional =
          menuConfigs.stream().filter(menuConfig -> menuConfig.getMatcher().test(cell)).findAny();
      return optional.map(t -> Menu.builder().setCell(cell).setConfig(t).setTable(this));
    }).filter(Optional::isPresent).map(Optional::get);
  }

  private Validator<Table> createMenuConfigValidator() {
    return new BaseValidator<>(
        config.getTopMenuConfigs().stream().collect(ImmutableMap.toImmutableMap(topMenuConfig -> {
          Predicate<Table> predicate = table -> table.getTopMenus().stream().map(Menu::getConfig)
              .filter(topMenuConfig::equals).findAny().isPresent();
          return predicate;
        }, topMenuConfig -> {
          Function<Table, String> function = table -> "没有匹配到" + topMenuConfig.getId() + "对应的菜单";
          return function;
        })));
  }

  public Optional<Menu> findMenu(String menuName) {
    return getMenus().filter(menu -> menu.getName().equals(menuName)).findAny();
  }

  public Stream<Menu> getMenus() {
    return topMenus.stream().map(MENU_TRAVERSER::breadthFirst).flatMap(Streams::stream);
  }

  public Stream<Menu> getDataMenus() {
    return getMenus().filter(Menu::isDataMenu);
  }

  // public Stream<Menu> getFixedDataMenus() {
  // return getDataMenus().filter(Menu::isFixedDataMenu);
  // }

  public Stream<Menu> getFixedDataMenus() {
    return getDataMenus().filter(Menu::isSingleDataMenu);
  }

  public Stream<Menu> getUnFixedDataMenus() {
    return getDataMenus().filter(Menu::isUnFixedDataMenu);
  }

  public Stream<Menu> getMustMenus() {
    return getMenus().filter(Menu::isMustMenu);
  }

  public Stream<Menu> getNotMustMenus() {
    return getMenus().filter(Menu::isNotMustMenu);
  }

  public ImmutableList<ValidationError<Table>> getAllErrors() {
    return Streams
        .concat(errors.stream(),
            topMenus.stream().map(Menu::getAllErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<Table>(this, error.getMessage())),
            data.getErrors().stream()
                .map(error -> new ValidationError<Table>(this, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public TableConfig getConfig() {
    return config;
  }

  public ImmutableList<ValidationError<Table>> getErrors() {
    return errors;
  }

  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }

  public ImmutableCollection<Menu> getTopMenus() {
    return topMenus;
  }

  public TableData getData() {
    return data;
  }
}
