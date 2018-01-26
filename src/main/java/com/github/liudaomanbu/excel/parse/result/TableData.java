package com.github.liudaomanbu.excel.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import com.github.liudaomanbu.excel.config.TableDataConfig;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class TableData<T> {
  public class Data {
    private final Map<Menu<T>, StandardCell> menuToValueCells;
    private final Map<String, Object> json;
    private final T value;

    private Data(Map<Menu<T>, StandardCell> menuToValueCells) {
      this.menuToValueCells = menuToValueCells;
      json = ExcelUtil.toJsonObject(menuToValueCells);
      value = ExcelUtil.toJavaObject(menuToValueCells, config.getType());
    }

    public Menu<T> getMenuByFieldName(String fieldName) {
      return menuToValueCells.keySet().stream()
          .filter(menu -> menu.getFieldName().equals(fieldName)).findAny().orElse(null);
    }

    public Map<Menu<T>, StandardCell> getMenuToValueCells() {
      return menuToValueCells;
    }

    public Map<String, Object> getJson() {
      return json;
    }

    public T getValue() {
      return value;
    }
  }

  private static final Joiner JOINER = Joiner.on("").skipNulls();

  private final Table<T> table;
  private final TableDataConfig<T> config;
  private final ImmutableList<Data> datas;
  private final ImmutableList<ValidationError<TableData<T>>> errors;

  public TableData(Table<T> table) {
    this.table = table;
    this.config = table.getConfig().getDataConfig();
    List<Map<Menu<T>, StandardCell>> menuTodatas = Lists.newArrayList();
    ImmutableList<Menu<T>> menus = table.getDataMenus().collect(ImmutableList.toImmutableList());

    ImmutableMap<Menu<T>, StandardCell> menuToFixedDatas =
        menus.stream().filter(Menu::isSingleDataMenu).collect(ImmutableMap.toImmutableMap(
            Function.identity(), menu -> Iterables.getOnlyElement(menu.getData().getValueCells())));
    menuTodatas.add(menuToFixedDatas);

    menus.stream().filter(Menu::isUnFixedDataMenu).forEach(menu -> {
      ImmutableList<StandardCell> valueCells = menu.getData().getValueCells();
      for (int j = 0; j < valueCells.size(); j++) {
        StandardCell valueCell = valueCells.get(j);

        Map<Menu<T>, StandardCell> map = null;
        if (j < menuTodatas.size()) {
          map = menuTodatas.get(j);
        } else {
          map = Maps.newHashMap(menuToFixedDatas);
          menuTodatas.add(map);
        }
        map.put(menu, valueCell);
      }
    });

    datas = menuTodatas.stream().map(Data::new).collect(ImmutableList.toImmutableList());

    Stream<ValidationError<TableData<T>>> menuMatcherErrors =
        datas.stream().map(t -> t.menuToValueCells).map(Map::entrySet).flatMap(Collection::stream)
            .flatMap(entry -> entry.getKey().getData().getConfig().getValidators().stream()
                .map(validator -> validator.validate(entry.getValue())).flatMap(Collection::stream)
                .map(error -> entry.getKey().getFullName() + error.getMessage()))
            .map(message -> new ValidationError<>(this, message));


    Stream<ValidationError<TableData<T>>> tableDataMatcherErrors = Optional.ofNullable(config)
        .map(TableDataConfig::getValidators).orElse(ImmutableList.of()).stream()
        .flatMap(validator -> datas.stream().map(map -> validator.validate(map))
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


  public ImmutableList<ValidationError<TableData<T>>> getErrors() {
    return errors;
  }

  public ImmutableList<Data> getDatas() {
    return datas;
  }

  public ImmutableList<Data> getEffectiveDatas() {
    return getDatas().stream().filter(data -> !data.json.values().isEmpty())
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<T> getValues() {
    return getEffectiveDatas().stream()
        .map(t -> (T) ExcelUtil.toJavaObject(t.menuToValueCells, config.getType()))
        .collect(ImmutableList.toImmutableList());
  }

  public List<Map<String, Object>> getJsons() {
    return getEffectiveDatas().stream().map(Data::getMenuToValueCells).map(ExcelUtil::toJsonObject)
        .collect(Collectors.toList());
  }
}
