package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.TableDataConfig;
import com.caotc.excel4j.parse.error.ValidationError;
import com.caotc.excel4j.util.ClassUtil;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.reflect.TypeToken;

public class TableData {
  private final Table table;
  private final TableDataConfig config;
  private final ImmutableList<Map<Menu, StandardCell>> menuToValueCells;
  private final ImmutableList<ValidationError<TableData>> errors;

  public TableData(Table table) {
    this.table = table;
    this.config = table.getConfig().getDataConfig();
    List<Map<Menu, StandardCell>> menuTodatas = Lists.newArrayList();
    ImmutableList<Menu> menus = table.getDataMenus().collect(ImmutableList.toImmutableList());
    for (int i = 0; i < menus.size(); i++) {
      Menu menu = menus.get(i);
      ImmutableList<StandardCell> valueCells = menu.getData().getValueCells();
      for (int j = 0; j < valueCells.size(); j++) {
        StandardCell valueCell = valueCells.get(j);

        Map<Menu, StandardCell> map = null;
        if (j < menuTodatas.size()) {
          map = menuTodatas.get(j);
        } else {
          map = Maps.newHashMap();
          menuTodatas.add(map);
        }
        map.put(menu, valueCell);
      }
    }

    menuToValueCells = menuTodatas
        .stream().filter(map -> map.values().stream().map(StandardCell::getValue)
            .filter(Objects::nonNull).findAny().isPresent())
        .collect(ImmutableList.toImmutableList());

    Stream<ValidationError<TableData>> menuMatcherErrors =
        menuToValueCells.stream().map(Map::entrySet).flatMap(Collection::stream)
            .flatMap(entry -> entry.getKey().getData().getConfig().getValidators().stream()
                .map(validator -> validator.validate(entry.getValue())).flatMap(Collection::stream)
                .map(error -> entry.getKey().getFullName() + error.getMessage()))
            .map(message -> new ValidationError<>(this, message));


    Stream<ValidationError<TableData>> tableDataMatcherErrors = Optional.ofNullable(config)
        .map(TableDataConfig::getValidators).orElse(ImmutableList.of()).stream()
        .flatMap(validator -> menuToValueCells.stream().map(map -> validator.validate(map))
            .flatMap(Collection::stream))
        .map(error -> new ValidationError<>(this, error.getMessage()));

    this.errors = Stream.concat(menuMatcherErrors, tableDataMatcherErrors)
        .collect(ImmutableList.toImmutableList());
  }

  public Table getTable() {
    return table;
  }

  public TableDataConfig getConfig() {
    return config;
  }

  public ImmutableList<Map<Menu, StandardCell>> getMenuToValueCells() {
    return menuToValueCells;
  }

  public ImmutableList<ValidationError<TableData>> getErrors() {
    return errors;
  }

  public <T> ImmutableList<T> getDatas(TypeToken<T> type) {
    ImmutableCollection<Field> fields =
        ClassUtil.getAllFields(type).collect(ImmutableSet.toImmutableSet());
    return menuToValueCells.stream().map(map -> {
      JSONObject jsonObject = new JSONObject();
      map.forEach((menu, cell) -> {
        Field field = menu.getField();
        if (Objects.isNull(field)) {
          field =
              fields.stream().filter(f -> f.getName().equals(menu.getFieldName())).findAny().get();
        }

        jsonObject.put(field.getName(),
            menu.getData().getConfig().getDataType().cast(cell.getValue(), field.getType()));
      });
      return (T) jsonObject.toJavaObject(type.getRawType());
    }).collect(ImmutableList.toImmutableList());
  }
}
