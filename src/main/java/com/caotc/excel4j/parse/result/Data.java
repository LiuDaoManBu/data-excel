package com.caotc.excel4j.parse.result;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.config.DataConfig;
import com.caotc.excel4j.config.MenuDataConfig;
import com.caotc.excel4j.parse.error.DataError;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Maps;

public class Data<V> {
  private final Menu<V> menu;
  private final MenuDataConfig<V> dataConfig;
  private final ImmutableList<DataError<V>> errors;
  private final ImmutableList<StandardCell> valueCells;

  public Data(Menu<V> menu) {
    super();
    this.menu = menu;
    this.dataConfig = menu.getMenuConfig().getDataConfig();
    this.valueCells = menu.getChildrens().isEmpty() ? dataConfig.getLoadType().getDataCells(menu)
        : ImmutableList.of();

    // TODO is DataError?
    errors = valueCells.stream().map(StandardCell::getValue).map(dataConfig.getDataMatcher()::match)
        .filter(Optional::isPresent).map(Optional::get)
        .map(message -> new DataError<V>(this, message)).collect(ImmutableList.toImmutableList());
  }

  // TODO
  // public JSONObject toJson() {
  // JSONObject result = new JSONObject();
  // menuToCells.asMap().forEach((key, values) -> {
  // ImmutableList<String> FieldNames = key.getFields();
  // JSONObject object=result;
  // for (int i = 0; i < FieldNames.size(); i++) {
  //
  // }
  // });
  // return result;
  // }

  // public Optional<V> getValue() {
  // V value = null;
  // // TODO
  // if (Objects.nonNull(dataConfig.getFieldName())) {
  // // TODO DataMenu时是否有返回值
  // // if (menu.isDataMenu()) {
  // // if (Objects.equals(ConstructType.OBJECT, dataConfig.getCastType()) &&
  // // menu.isFixedDataMenu()
  // // && dataConfig.getDataNumber() == 1) {
  // // value=dataConfig.cast(Iterables.getOnlyElement(valueCells).getValue());
  // // }
  // // value=dataConfig.cast(getCellValues());
  // // }else {
  // // value=dataConfig.getCastType().constructValue(menu);
  // // }
  // if (!menu.isDataMenu()) {
  // ImmutableList<Menu<?>> childrens = menu.getFieldChildrens();
  // Map<String, Object> vars = Maps.newHashMap();
  // // TODO 优化
  // childrens.stream().filter(Menu::isDataMenu).forEach(m -> {
  // vars.put((String) m.getFieldName().get(), menu.getData().getCellValues());
  // });
  // childrens.stream().filter(m -> !m.isDataMenu()).forEach(m -> {
  // vars.put((String) m.getFieldName().get(), menu.getData().getValue());
  // });
  // value = dataConfig.getConstructType().construct(dataConfig.getType(), vars,
  // menu.getMenuConfig().getEffectiveParserConfig());
  // }
  // }
  // return Optional.ofNullable(value);
  // }

  public ImmutableList<V> getCellValues() {
    // TODO
    return valueCells.stream().map(StandardCell::getValue).map(dataConfig::cast)
        .collect(ImmutableList.toImmutableList());
  }

  public Menu<V> getMenu() {
    return menu;
  }

  public DataConfig<V> getDataConfig() {
    return dataConfig;
  }

  public ImmutableList<StandardCell> getValueCells() {
    return valueCells;
  }

  public ImmutableList<DataError<V>> getErrors() {
    return errors;
  }

}
