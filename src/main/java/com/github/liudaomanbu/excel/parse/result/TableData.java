package com.github.liudaomanbu.excel.parse.result;

import com.google.common.collect.Streams;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import com.github.liudaomanbu.excel.config.TableDataConfig;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.github.liudaomanbu.excel.validator.BaseValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class TableData<T> {
  private final Table<T> table;
  private final TableDataConfig<T> config;
  private final ImmutableList<Data<T>> datas;
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

    datas = menuTodatas.stream().map(map -> new Data<>(this,map))
        .collect(ImmutableList.toImmutableList());

//    this.errors = getValidators()
//        .flatMap(validator -> datas.stream().filter(validator::premise).map(validator::validate)
//            .flatMap(Collection::stream))
//        .map(error -> new ValidationError<>(this, error.getMessage()))
//        .collect(ImmutableList.toImmutableList());
    this.errors =ImmutableList.of();
  }

  private Stream<Validator<Data<T>>> getValidators() {
    return  Optional.ofNullable(config)
        .map(TableDataConfig::getValidators).map(Collection::stream).orElseGet(Stream::empty);
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

  public ImmutableList<Data<T>> getDatas() {
    return datas;
  }

  public ImmutableList<ValidationError<TableData<T>>> getAllErrors() {
    return Stream
        .concat(errors.stream(),
            datas.stream().map(Data::getErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<>(this, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<Data<T>> getEffectiveDatas() {
    return getDatas().stream().filter(data -> !data.getJson().values().isEmpty())
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<T> getEffectiveValues() {
    return getEffectiveDatas().stream()
        .map(t -> (T) ExcelUtil.toJavaObject(t.getMenuToValueCells(), config.getType()))
        .collect(ImmutableList.toImmutableList());
  }

  public List<Map<String, Object>> getEffectiveJsons() {
    return getEffectiveDatas().stream().map(Data::getMenuToValueCells).map(ExcelUtil::toJsonObject)
        .collect(Collectors.toList());
  }
}
