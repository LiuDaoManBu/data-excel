package com.github.liudaomanbu.excel.parse.result;

import java.util.Collection;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.github.liudaomanbu.excel.config.MenuConfig;
import com.github.liudaomanbu.excel.config.TableConfig;
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

public class Table<T> {
  public static class Builder<T> {
    private TableConfig<T> config;
    private SheetParseResult sheetParseResult;

    public Table<T> build() {
      return new Table<>(this);
    }

    public TableConfig<T> getConfig() {
      return config;
    }

    public Builder<T> setConfig(TableConfig<T> config) {
      this.config = config;
      return this;
    }

    public SheetParseResult getSheetParseResult() {
      return sheetParseResult;
    }

    public Builder<T> setSheetParseResult(SheetParseResult sheetParseResult) {
      this.sheetParseResult = sheetParseResult;
      return this;
    }
  }

  private final Traverser<Menu<T>> MENU_TRAVERSER =
      Traverser.forTree(new SuccessorsFunction<Menu<T>>() {
        @Override
        public Iterable<? extends Menu<T>> successors(Menu<T> node) {
          return node.getChildrens();
        }
      });

  public static <T> Builder<T> builder() {
    return new Builder<>();
  }

  private final TableConfig<T> config;
  private final ImmutableList<ValidationError<Table<T>>> errors;
  private final SheetParseResult sheetParseResult;
  private final ImmutableCollection<Menu<T>> topMenus;
  private final TableData<T> data;

  public Table(Builder<T> builder) {
    config = builder.config;
    sheetParseResult = builder.sheetParseResult;
    topMenus = loadTopMenus().map(Menu.Builder::build).collect(ImmutableSet.toImmutableSet());
    errors = Stream.of(createMenuConfigValidator()).filter(validator -> validator.premise(this))
        .map(validator -> validator.validate(this)).flatMap(Collection::stream)
        .collect(ImmutableList.toImmutableList());
    this.data = new TableData<>(this);
  }

  private Stream<Menu.Builder<T>> loadTopMenus() {
    ImmutableCollection<MenuConfig<T>> menuConfigs = config.getTopMenuConfigs();
    Sheet sheet = sheetParseResult.getSheet();
    return ExcelUtil.getCells(sheet).map(StandardCell::valueOf).map(cell -> {
      Optional<MenuConfig<T>> optional =
          menuConfigs.stream().filter(menuConfig -> menuConfig.getMatcher().test(cell)).findAny();
      return optional.map(t -> Menu.<T>builder().setCell(cell).setConfig(t).setTable(this));
    }).filter(Optional::isPresent).map(Optional::get);
  }

  private Validator<Table<T>> createMenuConfigValidator() {
    return new BaseValidator<>(
        config.getTopMenuConfigs().stream().collect(ImmutableMap.toImmutableMap(topMenuConfig -> {
          Predicate<Table<T>> predicate = table -> table.getTopMenus().stream().map(Menu::getConfig)
              .filter(topMenuConfig::equals).findAny().isPresent();
          return predicate;
        }, topMenuConfig -> {
          Function<Table<T>, String> function = table -> "没有匹配到" + topMenuConfig.getId() + "对应的菜单";
          return function;
        })));
  }

  public Optional<Menu<T>> findMenu(String menuName) {
    return getMenus().filter(menu -> menu.getName().equals(menuName)).findAny();
  }

  public Stream<Menu<T>> getMenus() {
    return topMenus.stream().map(MENU_TRAVERSER::breadthFirst).flatMap(Streams::stream);
  }

  public Stream<Menu<T>> getDataMenus() {
    return getMenus().filter(Menu::isDataMenu);
  }

  // public Stream<Menu> getFixedDataMenus() {
  // return getDataMenus().filter(Menu::isFixedDataMenu);
  // }

  public Stream<Menu<T>> getFixedDataMenus() {
    return getDataMenus().filter(Menu::isSingleDataMenu);
  }

  public Stream<Menu<T>> getUnFixedDataMenus() {
    return getDataMenus().filter(Menu::isUnFixedDataMenu);
  }

  public Stream<Menu<T>> getMustMenus() {
    return getMenus().filter(Menu::isMustMenu);
  }

  public Stream<Menu<T>> getNotMustMenus() {
    return getMenus().filter(Menu::isNotMustMenu);
  }

  public ImmutableList<ValidationError<Table<T>>> getAllErrors() {
    return Streams
        .concat(errors.stream(),
            topMenus.stream().map(Menu::getAllErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<Table<T>>(this, error.getMessage())),
            data.getErrors().stream()
                .map(error -> new ValidationError<Table<T>>(this, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public boolean hasError() {
    return !getAllErrors().isEmpty();
  }

  public TableConfig<T> getConfig() {
    return config;
  }

  public ImmutableList<ValidationError<Table<T>>> getErrors() {
    return errors;
  }

  public SheetParseResult getSheetParseResult() {
    return sheetParseResult;
  }

  public ImmutableCollection<Menu<T>> getTopMenus() {
    return topMenus;
  }

  public TableData<T> getData() {
    return data;
  }
}
