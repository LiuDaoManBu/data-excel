package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.config.TableDataConfig;
import com.caotc.excel4j.parse.error.Error;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Streams;
import com.google.common.reflect.TypeToken;

public class TableData<V> {
  private final Table<V> table;
  private final TableDataConfig<V> dataConfig;
  private final ImmutableList<Map<Menu<?>, StandardCell>> menuToValueCells;
  private final ImmutableList<Error<TableData<V>>> errors;

  public TableData(Table<V> table) {
    this.table = table;
    this.dataConfig = table.getTableConfig().getDataConfig();
    List<Map<Menu<?>, StandardCell>> menuTodatas = Lists.newArrayList();
    ImmutableList<Menu<?>> menus = table.getDataMenus().collect(ImmutableList.toImmutableList());
    for (int i = 0; i < menus.size(); i++) {
      Menu<?> menu = menus.get(i);
      ImmutableList<StandardCell> valueCells = menu.getData().getValueCells();
      for (int j = 0; j < valueCells.size(); j++) {
        StandardCell valueCell = valueCells.get(j);

        Map<Menu<?>, StandardCell> map = null;
        if (j < menuTodatas.size()) {
          map = menuTodatas.get(j);
        } else {
          map = Maps.newHashMap();
          menuTodatas.add(map);
        }
        map.put(menu, valueCell);
      }
    }

    menuToValueCells =
        menuTodatas.stream().filter(map -> !map.isEmpty()).collect(ImmutableList.toImmutableList());
    
    Stream<Error<TableData<V>>> menuMatcherErrors=menuToValueCells.stream().map(Map::entrySet).flatMap(Collection::stream)
        .map(entry -> entry.getKey().getData().getDataConfig().getMatcher().match(entry.getValue()))
        .filter(Optional::isPresent).map(Optional::get)
        .map(message -> new Error<TableData<V>>(this, message));
    Stream<Error<TableData<V>>> tableDataMatcherErrors=menuToValueCells.stream().map(map->dataConfig.getMatcher().match(map))
        .filter(Optional::isPresent).map(Optional::get).map(message -> new Error<TableData<V>>(this, message));
    this.errors = Streams.concat(menuMatcherErrors,tableDataMatcherErrors).collect(ImmutableList.toImmutableList());
  }

  public Table<V> getTable() {
    return table;
  }

  public TableDataConfig<V> getDataConfig() {
    return dataConfig;
  }

  public ImmutableList<Map<Menu<?>, StandardCell>> getMenuToValueCells() {
    return menuToValueCells;
  }

  public ImmutableList<Error<TableData<V>>> getErrors() {
    return errors;
  }

  public <T> ImmutableList<T> getDatas(TypeToken<T> type) {
    return menuToValueCells.stream().map(map -> {
      JSONObject jsonObject = new JSONObject();
      map.forEach((menu, cell) -> {
        jsonObject.put(menu.getFieldName().get(),
            menu.getData().getDataConfig().cast(cell.getValue()));
      });
      return (T) jsonObject.toJavaObject(type.getRawType());
    }).collect(ImmutableList.toImmutableList());
  }
}
