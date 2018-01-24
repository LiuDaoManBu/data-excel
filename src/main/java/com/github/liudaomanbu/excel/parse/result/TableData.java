package com.github.liudaomanbu.excel.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import com.github.liudaomanbu.excel.config.TableDataConfig;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class TableData<T> {
  private final Table<T> table;
  private final TableDataConfig<T> config;
  private final ImmutableList<Map<Menu<T>, StandardCell>> menuToValueCells;
  private final ImmutableList<ValidationError<TableData<T>>> errors;

  public TableData(Table<T> table) {
    this.table = table;
    this.config = table.getConfig().getDataConfig();
    List<Map<Menu<T>, StandardCell>> menuTodatas = Lists.newArrayList();
    ImmutableList<Menu<T>> menus = table.getDataMenus().collect(ImmutableList.toImmutableList());
    menus.stream().filter(Menu::isUnFixedDataMenu).forEach(menu -> {
      ImmutableList<StandardCell> valueCells = menu.getData().getValueCells();
      for (int j = 0; j < valueCells.size(); j++) {
        StandardCell valueCell = valueCells.get(j);

        Map<Menu<T>, StandardCell> map = null;
        if (j < menuTodatas.size()) {
          map = menuTodatas.get(j);
        } else {
          map = Maps.newHashMap();
          menuTodatas.add(map);
        }
        map.put(menu, valueCell);
      }
    });

    menus.stream().filter(Menu::isSingleDataMenu)
        .forEach(menu -> menuTodatas.forEach(menuTodata -> menuTodata.put(menu,
            Iterables.getOnlyElement(menu.getData().getValueCells()))));

    menuToValueCells = menuTodatas
        .stream().filter(map -> map.values().stream().map(StandardCell::getValue)
            .filter(Objects::nonNull).findAny().isPresent())
        .collect(ImmutableList.toImmutableList());

    Stream<ValidationError<TableData<T>>> menuMatcherErrors =
        menuToValueCells.stream().map(Map::entrySet).flatMap(Collection::stream)
            .flatMap(entry -> entry.getKey().getData().getConfig().getValidators().stream()
                .map(validator -> validator.validate(entry.getValue())).flatMap(Collection::stream)
                .map(error -> entry.getKey().getFullName() + error.getMessage()))
            .map(message -> new ValidationError<>(this, message));


    Stream<ValidationError<TableData<T>>> tableDataMatcherErrors = Optional.ofNullable(config)
        .map(TableDataConfig::getValidators).orElse(ImmutableList.of()).stream()
        .flatMap(validator -> menuToValueCells.stream().map(map -> validator.validate(map))
            .flatMap(Collection::stream))
        .map(error -> new ValidationError<>(this, error.getMessage()));

    this.errors = Stream.concat(menuMatcherErrors, tableDataMatcherErrors)
        .collect(ImmutableList.toImmutableList());
  }

  public Table<T> getTable() {
    return table;
  }

  public TableDataConfig<T> getConfig() {
    return config;
  }

  public ImmutableList<Map<Menu<T>, StandardCell>> getMenuToValueCells() {
    return menuToValueCells;
  }

  public ImmutableList<ValidationError<TableData<T>>> getErrors() {
    return errors;
  }

  public ImmutableList<T> getDatas() {
    return menuToValueCells.stream().map(map -> (T) ExcelUtil.toJavaObject(map, config.getType()))
        .collect(ImmutableList.toImmutableList());
  }

  public List<Map<String, Object>> getJsonDatas() {
    return menuToValueCells.stream().map(ExcelUtil::toJsonObject).collect(Collectors.toList());
  }
}
